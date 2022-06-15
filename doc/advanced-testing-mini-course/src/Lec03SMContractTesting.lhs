Consumer-driven contract testing using state machines
=====================================================

Motivation
----------

- Components rarely exist in isolation, they almost always depend on some other
  component;

- When we test we often want to test as if the component existed in isolation
  though, e.g. if component A depends on component B, we'd like to test B first
  and then *assume* that B is working when testing A;

- Assumptions like these can be justified using so called *contract tests*.

Plan
----

- Following the pattern from lecture 1: make a SM based fake for B, use the fake
  as model to SM test the real implementation of B;

- Use the fake of B in place of the real implementation of B inside the real
  implementation of A;

- Make a SM model for A which contains the model of B and test the real
  implementaiton of A.

How it works
------------

```
               Interface
                   |
    Consumer       |     Producer
                   |
       ----------> x-------->
                   |
  "Collaboration   |  Contract tests
      tests"       |

```

SUT B: a queue (producer of the interface)
------------------------------------------

> module Lec03SMContractTesting where

> import Lec03.QueueInterface ()
> import Lec03.Queue ()
> import Lec03.QueueTest ()


SUT A: web service (consumer of the interface)
----------------------------------------------

> import Lec03.Service ()
> import Lec03.ServiceTest ()


Consumer-driven contract tests
------------------------------

The job of contract tests are to ensure the accuracy of the mocks/test doubles
you use of other components in your fast and deterministic integration tests.

Consumer-driven contract tests just means that the consumer of the mocked API
writes the contract test inside the testsuite of the producer.

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

Why not just spin up the real component B when testing component A?

- Imagine B is a queue and the real implementation uses Kafka, then we'd need to
  start several processes...

- Sometimes component B is slow to use (uses disk or network I/O)...

- Sometimes component B is a third-party component which we can't redeploy or
  reset between test runs...

- Often we want to be resilient at the level of component A in case component B
  fails, injecting faults in B to test this is much easier on a fake of B rather
  than on the real implementation of B (more on this in the next lecture).

- Basically this is how the road towards slow and flaky tests starts. Don't go
  down that path! If you are thinking: "but some code is only exercised when the
  real component is deployed, e.g. configuration", then use [smoke
  tests](https://en.wikipedia.org/wiki/Smoke_testing_%28software%29) rather than
  integration tests with real components.

  Origin of the terminology: "The phrase smoke test comes from electronic
  hardware testing. You plug in a new board and turn on the power. If you see
  smoke coming from the board, turn off the power. You don’t have to do any more
  testing."

  The software analogue: spin up component(s), wait for their status to become
  "ready", make some basic requests and see if they succeed.

  Acceptable if these are a bit flaky:

    + Component spin up happens relatively rarely in production
    + These tests will likely involve docker containers and networking, i.e.
      third-party infrastructure that sometimes is flaky

  "After code reviews, smoke testing is the most cost effective method for
  identifying and fixing defects in software." --
  [Microsoft](https://docs.microsoft.com/en-us/previous-versions/ms182613(v=vs.80))

  For most software systems, between good contract tests and smoke tests there
  shouldn't be much of a gap for bugs to sneak in. For special cases, such as
  distributed systems, we will cover more comprehensive techniques in lecture 5.

Exercises
---------

0. The fake/model of the queue is thread-safe, but the real implementation
   isn't! Fix that and do concurrent contract testing.

1. Introduce an interface for all database interaction, move the current
   database implementation to `realDb` and introduce fake database instance of
   the interface.

2. Write contract tests that ensure that the fake database faithfully represents
   the real one.

3. Once the contract tests pass, switch out the real database for the fake one
   in the collabortation tests (the testsuite of the web service). Enable timing
   output in `ghci` with `:set +s`, crank up the number of tests that
   `QuickCheck` generates, and see if you notice any speed up in the test
   execution time.

See also
--------

- For the difference between a fake and e.g. a mock see the following
  [article](https://www.martinfowler.com/bliki/TestDouble.html) by Martin
  Fowler;

- For more on contract testing see this
  [article](https://martinfowler.com/bliki/ContractTest.html) and for more on
  their consumer-driven variant see the following
  [artcile](https://martinfowler.com/articles/consumerDrivenContracts.html);

- [*Integrated Tests Are A Scam*](https://www.youtube.com/watch?v=fhFa4tkFUFw)
  talk by J.B. Rainsberger (2022), this a less ranty version of a talk with the
  same title that he [gave](https://www.youtube.com/watch?v=VDfX44fZoMc) at
  DevConFu in 2013.

Summary
-------

- Using fakes enables to fast and determinstic integration tests and, as we
  shall see next, makes it easier to introduce faults when testing;

- Contract tests justify the use of fakes, inplace of the real dependencies,
  when testing a SUT.
