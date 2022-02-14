## Advanced property-based testing mini-course

* Goals:
  - Show how to test stateful (i.e. impure/monadic) programs using
    property-based testing in general;
  - Show how to use fault injection and so called simulation testing to test
    distributed systems in particular;
  - Introduce the reader to related work and open problems in the area.

* Pre-requisites:
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

### Table of contents

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

### 1. State machine testing

* Motivation

  - Testing: "the process of using or trying something to see if it works, is
    suitable, obeys the rules, etc." -- Cambridge dictionary

  - In order to check that the software under test (SUT) obeys the rules we must
    first write the rules down

  - State machine specifications are one of many ways to formally "write down
    the rules"

```haskell
newtype Counter = Counter (IORef Int)

incr :: Counter -> IO ()
incr = undefined

get :: Counter -> IO Int
get = undefined
```

```haskell
newtype FakeCounter = FakeCounter Int

fakeIncr :: FakeCounter -> (FakeCounter, ())
fakeIncr (FakeCounter i) = (FakeCounter (i + 1), ())

fakeGet :: FakeCounter -> (FakeCounter, Int)
fakeGet (FakeCounter i) = (FakeCounter i, i)
```


```haskell
data Command = Incr | Get

data Response = Unit () | Int Int

type Model = FakeCounter

step :: Model -> Command -> (Model, Response)
step m cmd = case cmd of
  Incr -> Unit <$> fakeIncr m
  Get  -> Int  <$> fakeGet m

exec :: Counter -> Command -> IO Response
exec = undefined

newtype Program = Program [Command]

genProgram :: Model -> Gen Program
genProgram = undefined

prop_counter :: Property
prop_counter = forAll genProgram $ \(Program cmds) -> do
  undefined
```

* Regression tests

```haskell
runProgram :: Program -> IO Bool
runProgram = undefined

assertProgram :: String -> Program -> Assertion
assertProgram msg prog = do
  b <- runProgram prog
  assertBool msg b

```

* Excerises

1. Implement shrinking for programs.

2. Collect timing information about how long each command takes to execute on
   average.

* See also

  - Why state machines over other forms of specifications?
    + Executable (we will use this later)
    + Gurevich's generalisation of the Church-Turing thesis
    + Already heavily used in distributed systems

### 2. Concurrent state machine testing with linearisability

* Motivation

  - In the previous chapter we saw how to test if a sequential (single-threaded)
    program respects some state machine specification

  - Next we show how the *same* specification can be used to check if a
    concurrent execution is correct using linearisability

  - E.g. counters are often shared among different threads, how can we test that
    the counter implementation is thread-safe?

```haskell
newtype Program = Program [Command]

newtype ConcProgram = ConcProgram [[Command]]

genConcProgram :: Model -> Gen ConcProgram
genConcProgram = undefined

validConcProgram :: Model -> Precondition -> ConcProgram -> Bool
validConcProgram = undefined

data History = History Op

execConc :: ConcProgram -> IO History
execConc = undefined

linearisable :: History -> Bool
linearisable = undefined
```

### 3. Consumer-driven contract testing using state machines

* Motivation

  - Components rarely exist in isolation, they almost always depend on some
    other component;

  - When we test we often want to test as if the component existed in isolation,
    e.g. if component A depends on component B, we'd like to test B first and
    then *assume* that B is working when testing A;

  - Assumptions like these can be justified using so called *contract tests*.

### 4. Fault-injection

* Motivation
  - "almost all (92%) of the catastrophic system failures are the result of
    incorrect handling of non-fatal errors explicitly signaled in software.
    [...] in 58% of the catastrophic failures, the underlying faults could
    easily have been detected through simple testing of error handling code." --
    [Simple Testing Can Prevent Most Critical Failures: An Analysis of
    Production Failures in Distributed Data-intensive
    Systems](http://www.eecg.toronto.edu/~yuan/papers/failure_analysis_osdi14.pdf)
    (2014) Yuan et al;

### 5. Simulation testing

* Motivation

  - "haven't tested foundation[db] in part because their testing appears to be
    waaaay more rigorous than mine." -- Kyle
    ["aphyr"](https://twitter.com/aphyr/status/405017101804396546) Kingsbury

```haskell
data Network = Network
  { deploy  :: Addr -> IO () -- bind and listen
  , connect :: Addr -> IO ()
  , select  :: [(Addr, Msg)] -> IO (Addr, Msg, Time) -- send, accept and recv
  }

eventLoop :: Network -> [(Addr, StateMachine)] -> IO ()
eventLoop nw nodes = do
  socks <- mapM (deploy nw) (map fst nodes)
  connectAllNodesToEachOther nw nodes
  -- ^ Or do this as part of creating `Network`.
  let env = nodes `zip` initialStates
  go env []
    where
      go env outgoing = do
        (receiver, msg, time) <- select outgoing
        (outgoing', env') <- step env receiver msg time
        go env' outgoing'

fakeSend :: Heap -> Addr -> Msg -> (Heap, ())
fakeSend heap addr msg = do
  t <- genArrivalTime
  (enqueue (addr, msg, t) heap, ())

fakeRecv :: Heap -> (Heap, (Addr, Msg, Time))
fakeRecv = dequeue -- XXX: partial function

newFakeNetwork :: IO Network
newFakeNetwork = do
  heap <- newIORef emptyHeap
  let select outgoing = do
        h <- readIORef heap
        let h' = fakeSendAll h outgoing
            (h'', incoming) = fakeRecv h'
        writeIORef heap h''
        return incoming
  ...
  return Network {..}
```