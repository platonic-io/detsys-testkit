## Advanced property-based testing mini-course

### Table of content

0. Assume familiarity with Haskell and QuickCheck
1. State machine testing
  - Pre-conditions
  - Coverage
  - Execution trace for counterexamples
  - Regression tests from counterexamples
  - Metrics
  - References?
2. Consumer-driven contract tests using state machines
3. Concurrent state machine testing with linearisability
4. Fault-injection
5. Simulation testing

### Simulation testing

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
