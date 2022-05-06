Advanced property-based testing mini-course
===========================================

+ Goals:
  - Show how to test stateful (i.e. impure/monadic) programs using
    property-based testing in general;
  - Show how to use fault injection and so called simulation testing to test
    distributed systems in particular;
  - Introduce the reader to related work and open problems in the area.

+ Pre-requisites:

  - Enough familiarity with Haskell to be able to read simple programs, for
    example if you can follow along in the *Learn You a Haskell for Great Good!*
    [tutorial](http://learnyouahaskell.com/chapters), then you should be fine;

  - Basic knowledge of state machines (i.e.
    [Mealy](https://en.wikipedia.org/wiki/Mealy_machine) / [Moore
    machines](https://en.wikipedia.org/wiki/Moore_machine) and
    [transducers](https://en.wikipedia.org/wiki/Finite-state_transducer)).

  - Some experience with property-based testing of non-stateful (i.e. pure)
    programs. For example as explained in the official QuickCheck
    [manual](http://www.cse.chalmers.se/~rjmh/QuickCheck/manual.html) or in the
    following
    [tutorial](https://begriffs.com/posts/2017-01-14-design-use-quickcheck.html).

Structure
---------

Each lecture has the following structure:

- Motiviation: explains why we are doing what we are about to do;
- Plan: how we will do it;
- Code: a concrete implementation of the idea (in case you get stuck when trying
  to implement it yourself);
- Discussion: common questions or objections;
- Exercises: things the authors were to lazy to do, but they know how to;
- Problems: things the authors don't know how to do (yet);
- See also: links to further reading about the topic or related topics;
- Summary: the most important take away.

The lectures build upon each other. We start by modelling and testing a simple
counter using a state machine in lecture 1, we then reuse the same state machine
model to test the counter of thread-safety using linearisability in lecture 2.
In lecture 3 we will implement a queue and a web service that uses said queue,
the state machine model for the queue and the real implementation of the queue
will be contract tested to ensure that the model is faithful to the
implementation, subsequently while testing the web service we will use the model
in place of the real queue. In lecture 4 we introduce fault injection to the
queue allowing us to test how the web service performs when its dependency
fails. Finally, in lecture 5, we combine all the above ideas in what, sometimes
is called simulation testing, to test a distributed system that uses replicated
state machines.

Table of contents
-----------------

1. State machine testing
  - State machine models
  - Pre-conditions
  - Coverage
  - Execution trace for counterexamples
  - Regression tests from counterexamples
  - Metrics
  - References?
2. Concurrent state machine testing with linearisability
  - Generalise generation and execution to N threads
  - Collect history
  - Enumerate all possible sequential executions from concurrent history
  - Write simple linearisability checker: check if there's any such sequential
    execution that satisifies the (sequential) state machine model
3. Consumer-driven contract tests using state machines
4. Fault-injection
5. Simulation testing

> module Lec00Introduction where

> import Lec01SMTesting ()
> import Lec02ConcurrentSMTesting ()
> import Lec03SMContractTesting ()
> import Lec04FaultInjection ()
> import Lec05SimulationTesting ()
