> module ATMC.Lec5SimulationTesting where

> import ATMC.Lec5.EventLoop

Simulation testing
==================

Motivation
----------

  - "haven't tested foundation[db] in part because their testing appears to be
    waaaay more rigorous than mine." -- Kyle
    ["aphyr"](https://twitter.com/aphyr/status/405017101804396546) Kingsbury

  - ["Jepsen-proof engineering"](https://sled.rs/simulation.html)

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


Exercises
---------

0. Add a way to record all inputs during production deployment
1. Add a way to produce a history from the recorded inputs
2. Add a debugger that works on the history, similar to the REPL from the first
   lecture

3. Write a checker that works on histories that ensures that the safety
   properites from section 8 on correctness from [*Viewstamped Replication
   Revisited*](https://pmg.csail.mit.edu/papers/vr-revisited.pdf) by Barbara
   Liskov and James Cowling (2012)
