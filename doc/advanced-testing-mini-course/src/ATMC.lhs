> module ATMC where

Advanced property-based testing mini-course
===========================================

+ Goals:
  - Show how to test stateful (i.e. impure/monadic) programs using
    property-based testing in general;
  - Show how to use fault injection and so called simulation testing to test
    distributed systems in particular;
  - Introduce the reader to related work and open problems in the area.

+ Pre-requisites:
  - Enough familiarity with Haskell to be able to read simple programs;
  - Basic knowledge of state machines (i.e. Mealy and Moore machines)

    + https://en.wikipedia.org/wiki/Finite-state_transducer
    + [Computation and State
      Machines](https://www.microsoft.com/en-us/research/publication/computation-state-machines/)
      (2008) by Leslie Lamport

  - Some experience with property-based testing of non-stateful (i.e. pure)
    programs.
    + The original paper: [QuickCheck: a lightweight tool for random testing of
      Haskell
      programs](http://www.cs.tufts.edu/~nr/cs257/archive/john-hughes/quick.pdf)
      (2000) by Koen Claessen and John Hughes

Table of contents
-----------------

> import ATMC.Lec1SMTesting

1. State machine testing
  - State machine models
  - Pre-conditions
  - Coverage
  - Execution trace for counterexamples
  - Regression tests from counterexamples
  - Metrics
  - References?

> import ATMC.Lec2ConcurrentSMTesting

2. Concurrent state machine testing with linearisability
  - Generalise generation and execution to N threads
  - Collect history
  - Enumerate all possible sequential executions from concurrent history
  - Write simple linearisability checker: check if there's any such sequential
    execution that satisifies the (sequential) state machine model

> import ATMC.Lec3SMContractTesting

3. Consumer-driven contract tests using state machines

> import ATMC.Lec4FaultInjection

4. Fault-injection

> import ATMC.Lec5SimulationTesting

5. Simulation testing
