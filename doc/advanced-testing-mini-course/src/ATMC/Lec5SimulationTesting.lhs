> module ATMC.Lec5SimulationTesting where

Simulation testing
==================

Motivation
----------

  - "haven't tested foundation[db] in part because their testing appears to be
    waaaay more rigorous than mine." -- Kyle
    ["aphyr"](https://twitter.com/aphyr/status/405017101804396546) Kingsbury

Idea
----

  + What interface does the event loop provide?
    - Send/recv messages
    - Timers
    - File I/O
  + Implement this interface twice:
    1. Real implementation
    2. Simulation, where outgoing messages are intercepted and scheduled at
       random but deterministically using a seed (or dropped to simulate e.g.
       network partitions)

![simulation event loop](images/simulation-eventloop.svg)

The following pseudo code implementation of the event loop works for both the
"real" deployment and simulation:

```
while true {
  msg := deliver()
  out := recv(msg)
  send(out)
}
```

by switching out the implementation of `send` from sending messages over the
network to merely adding the message to priority queue, and switching out the
implementation of `deliever` from listning on a socket to merely popping
messages off the priority queue, then we can switch between a "real" deployment
and simulation by merely switching between different implementations of the
event loops interface (`send` and `deliver`).


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
