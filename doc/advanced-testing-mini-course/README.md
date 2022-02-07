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

* The software under test (SUT)

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

genCommands :: Model -> Gen [Command]
genCommands = undefined

prop_counter :: Property
prop_counter = forAll genCommands $ \cmds -> do
  undefined
```

### 2. Concurrent state machine testing with linearisability

### 3. Consumer-driven contract testing using state machines

### 4. Fault-injection

### 5. Simulation testing

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
