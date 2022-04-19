``` {.haskell .literate}
module ATMC where
```

Advanced property-based testing mini-course
===========================================

-   Goals:
    -   Show how to test stateful (i.e. impure/monadic) programs using property-based testing in general;
    -   Show how to use fault injection and so called simulation testing to test distributed systems in particular;
    -   Introduce the reader to related work and open problems in the area.
-   Pre-requisites:
    -   Enough familiarity with Haskell to be able to read simple programs;

    -   Basic knowledge of state machines (i.e. Mealy and Moore machines)

        -   https://en.wikipedia.org/wiki/Finite-state\_transducer
        -   [Computation and State Machines](https://www.microsoft.com/en-us/research/publication/computation-state-machines/)
            (2008)  by Leslie Lamport

    -   Some experience with property-based testing of non-stateful (i.e. pure) programs.

        -   The original paper: [QuickCheck: a lightweight tool for random testing of Haskell programs](http://www.cs.tufts.edu/~nr/cs257/archive/john-hughes/quick.pdf)
            (2000)  by Koen Claessen and John Hughes

Table of contents
-----------------

``` {.haskell .literate}
import ATMC.Lec1SMTesting
```

1.  State machine testing

-   State machine models
-   Pre-conditions
-   Coverage
-   Execution trace for counterexamples
-   Regression tests from counterexamples
-   Metrics
-   References?

``` {.haskell .literate}
import ATMC.Lec2ConcurrentSMTesting
```

2.  Concurrent state machine testing with linearisability

-   Generalise generation and execution to N threads
-   Collect history
-   Enumerate all possible sequential executions from concurrent history
-   Write simple linearisability checker: check if there's any such sequential execution that satisifies the (sequential) state machine model

``` {.haskell .literate}
import ATMC.Lec3SMContractTesting
```

3.  Consumer-driven contract tests using state machines

``` {.haskell .literate}
import ATMC.Lec4FaultInjection
```

4.  Fault-injection

``` {.haskell .literate}
import ATMC.Lec5SimulationTesting
```

5.  Simulation testing

---

``` {.haskell .literate}
module ATMC.Lec1SMTesting where
```

``` {.haskell .literate}
import Control.Monad.IO.Class
import Data.IORef
import Test.QuickCheck
import Test.QuickCheck.Monadic
import Test.HUnit
```

State machine testing
=====================

Motivation
----------

-   Testing: "the process of using or trying something to see if it works, is suitable, obeys the rules, etc." -- Cambridge dictionary

-   In order to check that the software under test (SUT) obeys the rules we must first write down the rules

-   State machine specifications are one of many ways to formally "write down the rules"

SUT
---

``` {.haskell .literate}
newtype Counter = Counter (IORef Int)
```

``` {.haskell .literate}
newCounter :: IO Counter
newCounter = do
  ref <- newIORef 0
  return (Counter ref)
```

``` {.haskell .literate}
incr :: Counter -> IO ()
incr (Counter ref) = modifyIORef ref (+ 1)
```

``` {.haskell .literate}
get :: Counter -> IO Int
get (Counter ref) = readIORef ref
```

State machine model/specification/fake
--------------------------------------

``` {.haskell .literate}
newtype FakeCounter = FakeCounter Int
```

``` {.haskell .literate}
fakeIncr :: FakeCounter -> (FakeCounter, ())
fakeIncr (FakeCounter i) = (FakeCounter (i + 1), ())
```

``` {.haskell .literate}
fakeGet :: FakeCounter -> (FakeCounter, Int)
fakeGet (FakeCounter i) = (FakeCounter i, i)
```

``` {.haskell .literate}
data Command = Incr | Get
  deriving (Eq, Show)
```

``` {.haskell .literate}
data Response = Unit () | Int Int
  deriving (Eq, Show)
```

``` {.haskell .literate}
type Model = FakeCounter
```

``` {.haskell .literate}
initModel :: Model
initModel = FakeCounter 0
```

``` {.haskell .literate}
step :: Model -> Command -> (Model, Response)
step m cmd = case cmd of
  Incr -> Unit <$> fakeIncr m
  Get  -> Int  <$> fakeGet m
```

``` {.haskell .literate}
exec :: Counter -> Command -> IO Response
exec c cmd = case cmd of
  Incr -> Unit <$> incr c
  Get  -> Int  <$> get c
```

``` {.haskell .literate}
newtype Program = Program [Command]
  deriving Show
```

``` {.haskell .literate}
genProgram :: Model -> Gen Program
genProgram _m = Program <$> listOf (elements [Incr, Get])
```

``` {.haskell .literate}
prop_counter :: Property
prop_counter = forAll (genProgram initModel) $ \prog -> monadicIO $ do
  c <- run newCounter
  let m = initModel
  runProgram c m prog
```

``` {.haskell .literate}
runProgram :: MonadIO m => Counter -> Model -> Program -> m Bool
runProgram c0 m0 (Program cmds) = go c0 m0 cmds
  where
     go _c _m []           = return True
     go  c  m (cmd : cmds) = do
       resp <- liftIO (exec c cmd)
       let (m', resp') = step m cmd
       if resp == resp'
       then go c m' cmds
       else return False
```

Regression tests
----------------

``` {.haskell .literate}
assertProgram :: String -> Program -> Assertion
assertProgram msg prog = do
  c <- newCounter
  let m = initModel
  b <- runProgram c m prog
  assertBool msg b
```

Excerises
---------

0.  Add a `Reset` `Command` which resets the counter to its initial value.

1.  Implement shrinking for programs.

2.  Write a REPL for the state machine. Start with the initial state, prompt the user for a command, apply the provided command to the step function and display the response as well as the new state, rinse and repeat.

    (For a SUT as simple as a counter this doesn't make much sense, but when the SUT get more complicated it might make sense to develope the state machine specification first, demo it using something like a REPL or some other simple UI before even starting to implement the real thing.)

3.  Collect timing information about how long each command takes to execute on average.

See also
--------

-   Why state machines over other forms of specifications? E.g. unit test-suite.
    -   Executable (as the REPL exercise shows, but also more on this later)
    -   Mental model
    -   Gurevich's generalisation of the Church-Turing thesis
    -   Already heavily used in distributed systems (later we'll see how the model becomes the implementation)

---

``` {.haskell .literate}
module ATMC.Lec2ConcurrentSMTesting where
```

``` {.haskell .literate}
import Test.QuickCheck
```

Concurrent state machine testing with linearisability
=====================================================

Motivation
----------

-   In the previous chapter we saw how to test if a sequential (single-threaded) program respects some state machine specification

-   Next we show how the *same* specification can be used to check if a concurrent execution is correct using linearisability

-   E.g. counters are often shared among different threads, how can we test that the counter implementation is thread-safe?

``` {.haskell .literate}
data Command = C
data Model = M
data Op = O
type Precondition = Command -> Bool
```

``` {.haskell .literate}
newtype Program = Program [Command]
```

``` {.haskell .literate}
newtype ConcProgram = ConcProgram [[Command]]
```

``` {.haskell .literate}
genConcProgram :: Model -> Gen ConcProgram
genConcProgram = undefined
```

``` {.haskell .literate}
validConcProgram :: Model -> Precondition -> ConcProgram -> Bool
validConcProgram = undefined
```

``` {.haskell .literate}
data History = History Op
```

``` {.haskell .literate}
execConc :: ConcProgram -> IO History
execConc = undefined
```

``` {.haskell .literate}
linearisable :: History -> Bool
linearisable = undefined
```

---

``` {.haskell .literate}
module ATMC.Lec3SMContractTesting where
```

Consumer-driven contract testing using state machines
=====================================================

Motivation
----------

-   Components rarely exist in isolation, they almost always depend on some other component;

-   When we test we often want to test as if the component existed in isolation, e.g. if component A depends on component B, we'd like to test B first and then *assume* that B is working when testing A;

-   Assumptions like these can be justified using so called *contract tests*.

---

``` {.haskell .literate}
module ATMC.Lec4FaultInjection where
```

Fault-injection
===============

Motivation
----------

-   "almost all (92%) of the catastrophic system failures are the result of incorrect handling of non-fatal errors explicitly signaled in software. \[...\] in 58% of the catastrophic failures, the underlying faults could easily have been detected through simple testing of error handling code." -- [Simple Testing Can Prevent Most Critical Failures: An Analysis of Production Failures in Distributed Data-intensive Systems](http://www.eecg.toronto.edu/~yuan/papers/failure_analysis_osdi14.pdf)
    (2014)  Yuan et al;

---

``` {.haskell .literate}
module ATMC.Lec5SimulationTesting where
```

``` {.haskell .literate}
import ATMC.Lec5.EventLoop
```

Simulation testing
==================

Motivation
----------

-   "haven't tested foundation\[db\] in part because their testing appears to be waaaay more rigorous than mine." -- Kyle ["aphyr"](https://twitter.com/aphyr/status/405017101804396546) Kingsbury

-   ["Jepsen-proof engineering"](https://sled.rs/simulation.html)

Idea
----

-   What interface does the event loop provide?
    -   Send/recv messages
    -   Timers
    -   File I/O
-   Implement this interface twice:
    1.  Real implementation
    2.  Simulation, where outgoing messages are intercepted and scheduled at random but deterministically using a seed (or dropped to simulate e.g. network partitions)

![simulation event loop](images/simulation-eventloop.svg)

The following pseudo code implementation of the event loop works for both the "real" deployment and simulation:

    while true {
      msg := deliver()
      out := recv(msg)
      send(out)
    }

by switching out the implementation of `send` from sending messages over the network to merely adding the message to priority queue, and switching out the implementation of `deliever` from listning on a socket to merely popping messages off the priority queue, then we can switch between a "real" deployment and simulation by merely switching between different implementations of the event loops interface (`send` and `deliver`).

Exercises
---------

0.  Add a way to record all inputs during production deployment

1.  Add a way to produce a history from the recorded inputs

2.  Add a debugger that works on the history, similar to the REPL from the first lecture

3.  Write a checker that works on histories that ensures that the safety properites from section 8 on correctness from [*Viewstamped Replication Revisited*](https://pmg.csail.mit.edu/papers/vr-revisited.pdf) by Barbara

    (2012)  Liskov and James Cowling

---

