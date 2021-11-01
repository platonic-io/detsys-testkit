### hs-disruptor

`hs-disruptor` is a Haskell port of the [LMAX
Disruptor](https://github.com/LMAX-Exchange/disruptor), which is a high
performance inter-thread messaging library. The developers at LMAX, which
operates a financial exchange,
[reported](https://www.infoq.com/presentations/LMAX/) in 2010 that they could
process more than 100,000 transactions per second at less than 1 millisecond
latency.

At its core it's just a lock-free concurrent queue, but it also provides
building blocks for achieving several useful concurrent programming tasks that
typical queues don't (or at least don't make obvious how to do). The extra
features include:

  * Multi-cast (many consumers can in parallel process the same event);
  * Batching (both on producer and consumer side);
  * Back-pressure;
  * Sharding for scalability;
  * Dependencies between consumers.

It's also performs better than most queues, as we shall see further down.

### Example

```haskell
import Control.Concurrent
import Control.Concurrent.Async
import Disruptor.SP

main :: IO ()
main = do

  -- Create the shared ring buffer.
  let bufferCapacity = 128
  rb <- newRingBuffer bufferCapacity

  -- The producer keeps a counter and produces events that are merely the pretty
  -- printed value as a string of that counter.
  let produce :: Int -> IO (String, Int)
      produce n = return (show n, n + 1)

      -- The counter starts at zero.
      initialProducerState = 0

      -- No back-pressure is applied in this example.
      backPressure :: Int -> IO ()
      backPressure _ = return ()

  producer <- newEventProducer rb produce backPressure initialProducerState

  -- The consumer merely prints the string event to the terminal.
  let consume :: () -> String -> SequenceNumber -> EndOfBatch -> IO ()
      consume () event snr endOfBatch =
        putStrLn (event ++ if endOfBatch then " (end of batch)" else "")

      -- The consumer doesn't need any state in this example.
      initialConsumerState = ()

      -- Which other consumers do we need to wait for before consuming an event?
      dependencies = []

      -- What to do in case there are no events to consume?
      waitStrategy = Sleep 1

  consumer <- newEventConsumer rb consume initialConsumerState dependencies waitStrategy

  -- Tell the ring buffer which the last consumer is, to avoid overwriting
  -- events that haven't been consumed yet.
  setGatingSequences rb [ecSequenceNumber consumer]

  withEventProducer producer $ \ap ->
    withEventConsumer consumer $ \ac -> do
      threadDelay (3 * 1000 * 1000) -- 3 sec
      cancel ap
      cancel ac
```

You can run the above example with `cabal run readme-example`.

A couple of things we could change to highlight the features we mentioned in the
above section:

  1. Add a second consumer that saves the event to disk, this consumer would be
     slower than the current one which logs to the terminal, but we could use
     buffer up events in memory and only actually write when the end of batch
     flag is set to speed things up;

  2. We could also shard depending on the sequence number, e.g. have two slower
     consumers that write to disk and have one of them handle even sequence numbers
     while the other handles odd ones;

  3. The above producer writes one event at the time to the ring buffer, but
     since we know at which sequence number the last consumer is at we can
     easily make writes in batches as well;

  4. Currently the producer doesn't apply any back-pressure, in a more realistic
     example where the producer would, for example, create events from requests
     made to a http server we could use back-pressure to tell the http server to
     return status code 429 (too many requests);

  5. If we have one consumer that writes to the terminal and another one that
     concurrently writes to disk, we could add a third consumer that does
     something with the event only if it has both been logged and stored to disk
     (i.e. the third consumer depends on both the first and the second).

### How it works

The ring buffer is implemented using a bounded array, it keeps track of a
monotonically increasing sequence number and it knows its the capacity of the
array, so to find out where to write the next value by simply taking the modulus
of the sequence number and the capacity. This has several advantages over
traditional queues:

  1. We never remove elements when dequeing, merely overwrite them once we gone
     all way around the ring. This removes write
     [contention](https://en.wikipedia.org/wiki/Resource_contention) between the
     producer and the consumer, one could also imagine avoiding garbage
     collection by only allocating memory the first time around the ring;

  2. Using an array rather than linked list increasing
     [striding](https://en.wikipedia.org/wiki/Stride_of_an_array) due to
     [spatial
     locality](https://en.wikipedia.org/wiki/Locality_of_reference#Spatial_and_temporal_locality_usage).

The ring buffer also keeps track of up to which sequence number its last
consumer as consumed, in order to not overwrite events that haven't been handled
yet.

This also means that producers can ask how much capacity left a ring buffer has,
and do batched writes. If there's no capacity left the producer can apply
back-pressure upstream as appropriate.

Consumers need keep track of which sequence number they have processed, in order
to avoid having the ring buffer overwrite unprocessed events as already
mentioned, but this also allows consumers to depend on each other.

When a consumer is done processing an event, it asks the ring buffer for the
event at its next sequence number, the ring buffer then replies that either
there are no new events, in which case the consumer applies it wait strategy, or
the ring buffer can reply that there are new events, the consumer the handles
each one in turn and the last one will be have the end of batch flag set, so
that the consumer can effectively batch the processing.

### Performance

`hs-disruptor`, which hasn't been optimised much yet, is about 2x slower than
LMAX's Java version on their single-producer single-consumer
[benchmark](https://github.com/LMAX-Exchange/disruptor/blob/master/src/perftest/java/com/lmax/disruptor/sequenced/OneToOneSequencedThroughputTest.java) (1P1C)
(basically the above example) on a ~2 years old Linux laptop.

The same benchmark compared to other Haskell libraries:

  * 10.3x faster than
    [`Control.Concurrent.Chan`](https://hackage.haskell.org/package/base-4.15.0.0/docs/Control-Concurrent-Chan.html);

  * 8.3x faster than
    [`Control.Concurrent.STM.TBQueue`](https://hackage.haskell.org/package/stm/docs/Control-Concurrent-STM-TBQueue.html);

  * 1.7x faster than
    [`unagi-chan`](https://hackage.haskell.org/package/unagi-chan);

  * 25.5x faster than
    [`chaselev-deque`](https://hackage.haskell.org/package/chaselev-deque);

  * 700x faster than [`ring-buffer`](https://hackage.haskell.org/package/ring-buffer);

  * 1.5x slower than
    [`lockfree-queue`](https://hackage.haskell.org/package/lockfree-queue);

  * TODO: Compare with
    [`data-ringbuffer`](https://github.com/kim/data-ringbuffer/tree/master/src/Data/RingBuffer).

In the triple-producer single-consumer (3P1C)
[benchmark](https://github.com/LMAX-Exchange/disruptor/blob/master/src/perftest/java/com/lmax/disruptor/sequenced/ThreeToOneSequencedThroughputTest.java),
the Java version is 5x slower than the Java 1P1C case. And our 3P1C is 4.6x
slower than our 1P1C version and our 3P1C version is 2.7x slower than the Java
version.

The same benchmark compared to other Haskell libraries:

  * 1.3x faster than
    [`unagi-chan`](https://hackage.haskell.org/package/unagi-chan);

  * TODO: Add more multi-producer benchmark comparisons.

See the file [`benchmark.sh`](benchmark.sh) for full details about how the
benchmarks are run.

As always take benchmarks with a grain of salt, we've tried to make them as fair
with respect to each other and as true to the original Java versions as
possible. If you see anything that seems unfair, or if you get very different
results when trying to reproduce the numbers, then please file an issue.

### Contributing

Any kind of improvements are most welcome! Please consider first opening an
issue before embarking on any big changes though.

### See also

#### Presentations

  * [LMAX - How to Do 100K TPS at Less than 1ms
    Latency](https://www.infoq.com/presentations/LMAX/) by Martin Thompson (QCon
    2010);

  * [LMAX Disruptor and the Concepts of Mechanical
    Sympathy](https://youtube.com/watch?v=Qho1QNbXBso) by Jamie Allen (2011);

  * [Concurrent Programming with the
    Disruptor](https://youtube.com/watch?v=eTeWxZvlCZ8) by Trisha Gee (2013?);

  * [Disruptor 3.0: Details and Advanced
    Patterns](https://youtube.com/watch?v=2Be_Lqa35Y0) by Mike Barker (YOW!
    2013);

  * [Designing for Performance](https://youtube.com/watch?v=fDGWWpHlzvw) by
    Martin Thompson (GOTO 2015);

  * [Evolution of Financial Exchange
     Architectures](https://www.youtube.com/watch?v=qDhTjE0XmkE) by Martin
     Thompson (QCon 2020)
      + 1,000,000 tx/s and less than 100 microseconds latency, he is no longer
        at LMAX though so we don't know if these exchanges are using the
        disruptor pattern.

#### Writings

  * Martin Thompson's [blog](https://mechanical-sympathy.blogspot.com/);
  * The Disruptor [mailing list](https://groups.google.com/g/lmax-disruptor);
  * The Mechanical Sympathy [mailing list](https://groups.google.com/g/mechanical-sympathy);
  * [The LMAX Architecture](https://martinfowler.com/articles/lmax.html) by
    Martin Fowler (2011).

### License

Copyright (c) 2021 Symbiont Inc.

All rights reserved.

See the file [LICENSE](LICENSE).
