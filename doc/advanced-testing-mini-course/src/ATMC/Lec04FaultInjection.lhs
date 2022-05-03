> module ATMC.Lec04FaultInjection where

> import Control.Exception
> import Data.IORef

Fault-injection
===============

Motivation
----------

  - "almost all (92%) of the catastrophic system failures are the result of
    incorrect handling of non-fatal errors explicitly signaled in software.
    [...] in 58% of the catastrophic failures, the underlying faults could
    easily have been detected through simple testing of error handling code." --
    [Simple Testing Can Prevent Most Critical Failures: An Analysis of
    Production Failures in Distributed Data-intensive
    Systems](http://www.eecg.toronto.edu/~yuan/papers/failure_analysis_osdi14.pdf)
    (2014) Yuan et al;

Plan
----

- Possible faults to inject for the queue
   + write fails, e.g. queue is full
   + read fails, e.g. bug in queue causes exception to be thrown
   + read returns a malformed write which no longer deserialises, or has a valid
     client request id to send the response to

> import ATMC.Lec03.QueueInterface
> import ATMC.Lec03.Service

> data FaultyFakeQueue a = FaultyFakeQueue
>   { ffqQueue :: QueueI a
>   , ffqFault :: IORef (Maybe Fault)
>   }

> data Fault = Full | Empty | ReadFail IOException

> faultyFakeQueue :: Int -> IO (FaultyFakeQueue a)
> faultyFakeQueue size = do
>   fake <- fakeQueue size
>   ref  <- newIORef Nothing
>   return FaultyFakeQueue
>     { ffqQueue = QueueI
>         { qiEnqueue = enqueue fake ref
>         , qiDequeue = dequeue fake ref
>         }
>     , ffqFault = ref
>     }
>   where
>     enqueue fake ref x = do
>       fault <- readIORef ref
>       case fault of
>         Just Full  -> return False
>         _otherwise -> qiEnqueue fake x

>     dequeue fake ref = do
>       fault <- readIORef ref
>       case fault of
>         Just Empty          -> return Nothing
>         Just (ReadFail err) -> throwIO err
>         _otherwse           -> qiDequeue fake

> injectFullFault :: FaultyFakeQueue a -> IO ()
> injectFullFault (FaultyFakeQueue _queue ref) = writeIORef ref (Just Full)

> injectEmptyFault :: FaultyFakeQueue a -> IO ()
> injectEmptyFault (FaultyFakeQueue _queue ref) = writeIORef ref (Just Empty)

> injectReadFailFault :: FaultyFakeQueue a -> IOException -> IO ()
> injectReadFailFault (FaultyFakeQueue _queue ref) err = writeIORef ref (Just (ReadFail err))

> removeFault :: FaultyFakeQueue a -> IO ()
> removeFault (FaultyFakeQueue _queue ref) = writeIORef ref Nothing

> test_injectFullFault :: IO ()
> test_injectFullFault = do
>   ffq <- faultyFakeQueue 4
>   res1 <- qiEnqueue (ffqQueue ffq) "test1"
>   assert (res1 == True) (return ())
>   injectFullFault ffq
>   res2 <- qiEnqueue (ffqQueue ffq) "test2"
>   assert (res2 == False) (return ())

Discussion
----------

- Can we not just inject real faults like Jepsen does?
  [`iptables`](https://linux.die.net/man/8/iptables) for dropping messages and
  network partitions, [`tc`](https://man7.org/linux/man-pages/man8/tc.8.html) for
  creating latency or simulating a slow connection on the network,
  [`(p)kill`](https://linux.die.net/man/1/kill) for killing processes, `kill -STOP
  $pid` and `kill -CONT $pid` for pausing and resuming processes to simulate long
  I/O or GC pauses, [`libfaketime`](https://github.com/wolfcw/libfaketime) for
  clock-skews, etc?

  We could, after all Jepsen is a very successful at finding bugs in distributed
  databases using these techniques. However keep in mind exactly how Jepsen is
  used: typically companies hire Kyle Kingsbury for a couple of weeks/months, he
  writes the tests and runs them, analyses the results and writes a report.

  XXX:

  * requires root, needs to be done in containers or vm which slows down and
    complicates start up
  * non-deterministic
  * slow
  * ci flakiness
  * blackbox

- Can we contract test the fault injection? I.e. how do we know that the faults
  we inject correspond to real faults that can happen? How can we be sure to
  have covered all possible real faults?

  XXX:

  * fault models, e.g. see: https://github.com/coilhq/tigerbeetle/blob/main/docs/DESIGN.md#fault-models

Exercises
---------

0. Try to imagine how much more difficult it would be to write these tests
   without injecting the faults in the fake, but rather the real dependency.


See also
--------

- [*Why Is Random Testing Effective for Partition Tolerance
  Bugs?*(https://dl.acm.org/doi/pdf/10.1145/3158134) by Majumdar and Niksic
  (2018)
