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

SUT with real queue
-------------------

The SUT of the day is a web service which queues up client requests and has a
worker that processes the queue and replies to the clients.

![](./images/lec3-web-service-with-queue-small.jpg)

Imagine if this queue is a separate process. This makes it a bit annoying to
test because we need to deploy the queue first, make sure it's ready for work
before we start testing the web service.

SUT with interface
------------------

One way around the above problem is to implement the web service against an
*interface* of the queue rather than the queue itself. We can then implement
this interface using the real queue but also a fake queue which lives in the
same process as the web service hence avoiding deploying the queue before
testing. Depending if we deploy the web service in "production" or for "testing"
we choose the between the two implementations of the interface.

![](./images/lec3-web-service-with-interface-small.jpg)

The problem of this approach is: how do we know that the fake queue is faithful
to the real queue implementation? We would need to test this somehow! (These
tests are usually called contract tests.)

Recall: SM testing
------------------

Let's take a step back and recall what we are doing when we are state machine
testing. We ensure that the state machine model is faithful to the SUT.


![](./images/lec3-sm-model-small.jpg)

SM model fake
-------------

Assuming we have a state machine model of the queue which we know is faithful to
the real queue, is there a way to turn this model into a fake and hence solve
our problem?

Yes! It's quite simple, merely create a wrapper around the state machine model
which has a variable with the current state. Initialise this current state with
the initial model, and every time we get an input, read the state, apply the
state machine function, update the state variable.

![](./images/lec3-sm-model-fake-small.jpg)

"Collaboration tests" vs contract tests
---------------------------------------

Let's zoom out a bit and contemplate the general picture. Our queue can be
thought of as a producer of the interface, while the web service is consumer of
it.

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

When we test our web service against the fake queue we are doing, what is
sometimes called, "collaboration tests", while when we are ensuring that the
fake queue is faithful to the real queue we are doing contract tests.

The above relations between consumers and producers of interfaces can be
generalised from one-to-one relations, as in the web service and queue example,
to many-to-many relations and we can also nest them, i.e. a producer can in turn
be a consumer. The kind of testing we've talked about generalised to these
contexts as well and done in "layers", starting with the bottom layer and going
up.

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
     consumer might have been broken.

Code
----

> module Lec03SMContractTesting where

> import Lec03.QueueInterface ()
> import Lec03.Queue ()
> import Lec03.QueueTest ()

> import Lec03.Service ()
> import Lec03.ServiceTest ()

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

4. Think of corner cases for the queue you'd write unit tests for, but instread
   add those cases to the coverage checker to ensure that the generator
   generates them.

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

- State machine testing a component using a model gives us a faithful fake for
  that component for free;

- Using fakes enables to fast and determinstic integration tests and, as we
  shall see next, makes it easier to introduce faults when testing;

- Contract tests justify the use of fakes, inplace of the real dependencies,
  when testing a SUT.
