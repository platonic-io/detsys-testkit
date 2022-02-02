## Advanced property-based testing mini-course

### Table of content

0. Assume familiarity with Haskell and QuickCheck
1. State machine testing
  - Coverage
  - Metrics
  - References?
2. Consumer-driven contract tests using state machines
3. Concurrent state machine testing with linearisability
4. Fault-injection
5. Simulation testing

### Simulation testing

```haskell
data Network = Network
  { deploy  :: Addr -> IO Server -- bind and listen
  , connect :: Addr -> IO Client
  , send    :: [(Client, Msg)] -> IO ()
  , select  :: [Server] -> IO (Addr, Msg, Time) -- accept and recv
  }

eventLoop :: Network -> [(Addr, StateMachine)] -> IO ()
eventLoop nw nodes = do
  servers <- mapM (deploy nw) (map fst nodes)
  let env = nodes `zip` initialStates
  go env
    where
      go env = do
        (receiver, msg, time) <- select servers
        (outgoing, env') <- step env receiver msg time
        send nw outgoing
        go env'

fakeSend :: Heap -> Addr -> Msg -> (Heap, ())
fakeSend heap addr msg = do
  t <- genArrivalTime
  (enqueue (addr, msg, t) heap, ())

newFakeNetwork :: IO Network
newFakeNetwork = do
  heap <- newIORef emptyHeap
  let send = do
        h <- readIORef heap
        let (h', ()) = fakeSend h
        writeIORef heap h'
  ...
  return Network {..}
```
