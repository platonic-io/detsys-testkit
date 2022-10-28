Property-based testing stateful systems
=======================================

Property-based testing (PBT) of pure programs is an established practice by now.
It's taught in introductory university classes and it's part of test suites in
industry.

Most real world programs are not pure though, they are stateful. While it's
often possible to structure your program in such a way that the impure stuff is
done in `main`, e.g. read the contents of a file, and then passed on to a pure
function, e.g. a parser, it's not always possible. Consider a long-running
program that interacts with the filesystem and with other programs over the
network, e.g. some kind of web service or a distributed database. It's difficult
to split such a program up into doing a little bit of impure stuff at the start,
then hand it over to a pure function (which we can apply PBT on).

Given this it's perhaps a bit surprising that there are relatively few resources
about applying PBT to stateful systems. This repository is an attempt to close
that gap and try to make PBT stateful systems more common.

The goals we'd like to achieve are:

  - Show how to test stateful (i.e. impure/monadic) programs using
    property-based testing in general;

  - Show how to use fault injection and so called simulation testing to test
    distributed systems in particular;

  - Introduce the reader to related work and open problems in the area.

In the interest of brevity, we assume that the reader already has:

  - Enough familiarity with Haskell to be able to read simple programs, for
    example if you can follow along in the *Learn You a Haskell for Great Good!*
    [tutorial](http://learnyouahaskell.com/chapters), then you should be fine;

  - Some experience with property-based testing of non-stateful (i.e. pure)
    programs. For example as explained in the official QuickCheck
    [manual](http://www.cse.chalmers.se/~rjmh/QuickCheck/manual.html) or in the
    following
    [tutorial](https://begriffs.com/posts/2017-01-14-design-use-quickcheck.html);

  - Basic knowledge of state machines (i.e.
    [Mealy](https://en.wikipedia.org/wiki/Mealy_machine) / [Moore
    machines](https://en.wikipedia.org/wiki/Moore_machine) and
    [transducers](https://en.wikipedia.org/wiki/Finite-state_transducer)).

Other than that this resource is striving to be as self-contained as possibly.

Structure
---------

The resource is split up into five parts (so far), and each part has the
following structure:

- Motiviation: explains why we are doing what we are about to do;
- Plan: how we will do it;
- Code: a concrete implementation of the idea (in case you get stuck when trying
  to implement it yourself);
- Discussion: common questions or objections;
- Exercises: things the authors were to lazy to do, but they know how to;
- Problems: things the authors don't know how to do (yet);
- See also: links to further reading about the topic or related topics;
- Summary: the most important take away.

The parts build upon each other. We start by modelling and testing a simple
counter using a state machine in part 1, we then reuse the same state machine
model to test the counter for thread-safety using linearisability in part 2. In
part 3 we will implement a queue and a web service that uses said queue, the
state machine model for the queue and the real implementation of the queue will
be contract tested to ensure that the model is faithful to the implementation,
subsequently while testing the web service we will use the model in place of the
real queue. In part 4 we introduce fault injection to the queue allowing us to
test how the web service performs when its dependency fails. Finally, in part 5,
we combine all the above ideas in what, sometimes is called simulation testing,
to test a distributed system that uses replicated state machines.

Table of contents
-----------------

1. State machine testing
2. Concurrent state machine testing with linearisability
3. Integration tests against state machine fakes and consumer-driven contract tests for the fakes
4. Fault-injection
5. Simulation testing

<!---
> module Lec00Introduction where

> import Lec01SMTesting ()
> import Lec02ConcurrentSMTesting ()
> import Lec03SMContractTesting ()
> import Lec04FaultInjection ()
> import Lec05SimulationTesting ()
--->
