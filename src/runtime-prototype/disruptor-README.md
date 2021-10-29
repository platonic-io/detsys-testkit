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
```

### Performance

`hs-disruptor`, which hasn't been optimised much yet, is about 2x slower than
LMAX's Java version on their single-producer single-consumer
[benchmark](https://github.com/LMAX-Exchange/disruptor/blob/master/src/perftest/java/com/lmax/disruptor/sequenced/OneToOneSequencedThroughputTest.java)
(basically the above example) on a ~2 years old laptop running Linux.

The same benchmark compared to other Haskell libraries:

  * 10.3x faster than
    [`Control.Concurrent.Chan`](https://hackage.haskell.org/package/base-4.15.0.0/docs/Control-Concurrent-Chan.html)

  * 8.3x faster than
    [`Control.Concurrent.STM.TBQueue`](https://hackage.haskell.org/package/stm/docs/Control-Concurrent-STM-TBQueue.html)

  * 1.7x faster than
    [`unagi-chan`](https://hackage.haskell.org/package/unagi-chan)

  * 25.5x faster than
    [`chaselev-deque`](https://hackage.haskell.org/package/chaselev-deque)

  * 700x faster than [`ring-buffer`](https://hackage.haskell.org/package/ring-buffer)

  * 1.5x slower than
    [`lockfree-queue`](https://hackage.haskell.org/package/lockfree-queue)


See the file [`benchmark.sh`](benchmark.sh) for full details about how the
benchmarks are run.

As always, take benchmarks with a grain of salt, we've tried to make them as
fair with respect to each other and as true to the original Java
[single-producer](https://github.com/LMAX-Exchange/disruptor/blob/master/src/perftest/java/com/lmax/disruptor/sequenced/OneToOneSequencedThroughputTest.java)
and
[multi-producer](https://github.com/LMAX-Exchange/disruptor/blob/master/src/perftest/java/com/lmax/disruptor/sequenced/ThreeToOneSequencedThroughputTest.java)
versions as possible.

### How it works

### Contributing

### See also

#### Presentations

  * [LMAX - How to Do 100K TPS at Less than 1ms
    Latency](https://www.infoq.com/presentations/LMAX/) by Martin Thompson (QCon
    2010)

  * [LMAX Disruptor and the Concepts of Mechanical
    Sympathy](https://youtube.com/watch?v=Qho1QNbXBso) by Jamie Allen (2011)

  * [Concurrent Programming with the
    Disruptor](https://youtube.com/watch?v=eTeWxZvlCZ8) by Trisha Gee (2013?)

  * [Disruptor 3.0: Details and Advanced
    Patterns](https://youtube.com/watch?v=2Be_Lqa35Y0) by Mike Barker (YOW!
    2013)

  * [Designing for Performance](https://youtube.com/watch?v=fDGWWpHlzvw) by
    Martin Thompson (GOTO 2015)

  * [Evolution of Financial Exchange
     Architectures](https://www.youtube.com/watch?v=qDhTjE0XmkE) by Martin
     Thompson (QCon 2020)
      + 1,000,000 tx / s and less than 100 microseconds latency, he is no longer
        at LMAX though so we don't know if these exchanges are using the
        disruptor pattern.

#### Writings

  * [The LMAX Architecture](https://martinfowler.com/articles/lmax.html) by
    Martin Fowler (2011)

#### Prior work in Haskell

Searching for "disruptor" or "queue" on
[GitHub](https://github.com/search?l=Haskell&q=disruptor&type=Repositories) and
[Hackage](https://hackage.haskell.org/packages/search?terms=queue) gives the
following hits at the time of writing:

##### Disruptor

  * https://github.com/kim/data-ringbuffer/tree/master/src/Data/RingBuffer
  * https://github.com/iand675/disruptor
  * https://github.com/maoe/disruptor/blob/develop/benchmarks/ping-pong-latency.hs
  * https://github.com/Tener/disruptor-hs


### License

Copyright (c) 2021 Symbiont Inc.

All rights reserved.
