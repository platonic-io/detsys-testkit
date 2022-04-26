> module ATMC.Lec4FaultInjection where

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

Exercises
---------

0. Try to imagine how much more difficult it would be to write these tests
   without injecting the faults in the fake, but rather the real dependency.
