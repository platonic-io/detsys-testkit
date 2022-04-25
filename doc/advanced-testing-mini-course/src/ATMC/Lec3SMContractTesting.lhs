> module ATMC.Lec3SMContractTesting where

Consumer-driven contract testing using state machines
=====================================================

Motivation
----------

  - Components rarely exist in isolation, they almost always depend on some
    other component;

  - When we test we often want to test as if the component existed in isolation
    though, e.g. if component A depends on component B, we'd like to test B
    first and then *assume* that B is working when testing A;

  - Assumptions like these can be justified using so called *contract tests*.

Plan
----

  - Following the pattern from lecture 1: make a SM based fake for B, use the
    fake as model to SM test the real implementation of B;

  - Use the fake of B in place of the real implementation of B inside the real
    implementation of A;

  - Make a SM model for A which contains the model of B and test the real
    implementaiton of A.

Picture
-------

XXX:

```
                interface     
                   |
  Consumer         |   /----\      Producer
          -------> x--+      +-->
                   |
                   |
```

SUT B
-----

> sutB = undefined

SUT A
-----

> sutA = undefined

Consumer-driven contract tests
------------------------------

If component A and B are developed in different repos or by different teams,
then the consumer of the API (in our case A consumes B's API) should write the
contract test (hence *consumer-driven*).

That way:

  1. the fake of the consumed API is more to encode the assumptions that the
     consumer makes;

  2. if the implementation of the consumed API changes in a way that break the
     contract test that ensures that the fake is faithfully with regards to the
     real implementation, then the developers of the consumed API will get a
     failing test and thus a warning about the fact that some assumptions of the
     comsumer might have been broken.

Discussion
----------

  - Why not just spin up the real component B when testing component A?

    + Imagine B is a queue and the real implementation uses Kafka, then we'd
      need to start several processes...

    + Sometimes component B is slow to use (uses disk or network I/O)...

    + Sometimes component B is a third-party component...

    + Often we want to be resilient at the level of component A in case
      component B fails, injecting faults in B to test this is much easier on a
      fake of B rather than on the real implementation of B (more on this in the
      next lecture).

See also
--------

* [*Integrated Tests Are A Scam*](https://www.youtube.com/watch?v=fhFa4tkFUFw)
  talk by J.B. Rainsberger (2022)
