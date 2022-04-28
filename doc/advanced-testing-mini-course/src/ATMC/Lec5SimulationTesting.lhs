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
- https://making.pusher.com/fuzz-testing-distributed-systems-with-quickcheck/
- https://fractalscapeblog.wordpress.com/2017/05/05/quickcheck-for-paxos/

See also
--------

- Where do these ideas come from?

  The first reference we been able to find is the following concluding remark by
  Alan Perlis (the first recipient of the Turing Award) about a discussion about
  a paper on simulation testing by Brian Randell at the first
  [conference](http://homepages.cs.ncl.ac.uk/brian.randell/NATO/nato1968.PDF) on
  Software Engineering (this is where we got the term from) in 1968:

    "I’d like to read three sentences to close this issue.

       1. A software system can best be designed if the testing is interlaced with
          the designing instead of being used after the design.

       2. A simulation which matches the requirements contains the control which
          organizes the design of the system.

       3. Through successive repetitions of this process of interlaced testing and
          design the model ultimately becomes the software system itself. I think that it
          is the key of the approach that has been suggested, that there is no such
          question as testing things after the fact with simulation models, but that in
          effect the testing and the replacement of simulations with modules that are
          deeper and more detailed goes on with the simulation model controlling, as it
          were, the place and order in which these things are done."

- The idea in its current shape and as applied to distributed systems was
  introduced(?), or at the very least popularised, by Will Wilson's talk
  [*Testing Distributed Systems w/ Deterministic
  Simulation*](https://www.youtube.com/watch?v=4fFDFbi3toc) at Strange Loop
  (2014) about how they used [simulation
  testing](https://apple.github.io/foundationdb/testing.html) to test
  [FoundationDB](https://www.foundationdb.org/) (so well that Kyle "aphyr"
  Kingsbury didn't feel it was
  [worth](https://twitter.com/aphyr/status/405017101804396546) Jepsen testing
  it).

  Watching the talk and rereading the Perlis quote makes one wonder: was the
  technique independently rediscovered, or had they in fact read the (in)famous
  1968 NATO software engineering report?

- There's also the more established practice of [discrete-event
  simulation](https://en.wikipedia.org/wiki/Discrete-event_simulation) which is
  usually used in different contexts than software testing, but nevertheless is
  close enough in principle that it's worth taking inspiration from (and indeed
  the simulation testing people often refer to it).

- Other users of simulation testing include.

    + AWS

Here's a quote from the recently published paper [Millions of Tiny
Databases](https://www.usenix.org/conference/nsdi20/presentation/brooker) (2020)
written by three AWS engineers:

 "To solve this problem [testing distributed systems], we picked an approach that
  is in wide use at Amazon Web Services, which we would like to see broadly
  adopted: build a test harness which abstracts networking, performance, and
  other systems concepts (we call it a simworld). The goal of this approach is to
  allow developers to write distributed systems tests, including tests that
  simulate packet loss, server failures, corruption, and other failure cases, as
  unit tests in the same language as the system itself. In this case, these unit
  tests run inside the developer’s IDE (or with junit at build time), with no need
  for test clusters or other infrastructure. A typical test which tests
  correctness under packet loss can be implemented in less than 10 lines of Java
  code, and executes in less than 100ms. The Physalia team have written hundreds
  of such tests, far exceeding the coverage that would be practical in any
  cluster-based or container-based approach.

  The key to building a simworld is to build code against abstract physical layers
  (such as networks, clocks, and disks). In Java we simply wrap these thin layers
  in interfaces. In production, the code runs against implementations that use
  real TCP/IP, DNS and other infrastructure. In the simworld, the implementations
  are based on in-memory implementa- tions that can be trivially created and torn
  down. In turn, these in-memory implementations include rich fault-injection
  APIs, which allow test implementors to specify simple statements like:
  `net.partitionOff ( PARTITION_NAME , p5.getLocalAddress () ); ...
  net.healPartition ( PARTITION_NAME );`

  Our implementation allows control down to the packet level, allowing testers
  to delay, duplicate or drop packets based on matching criteria. Similar
  capabilities are available to test disk IO. Perhaps the most important testing
  capability in a distributed database is time, where the framework allows each
  actor to have it’s own view of time arbitrarily controlled by the test.
  Simworld tests can even add Byzantine conditions like data corruption, and
  operational properties like high la- tency. We highly recommend this testing
  approach, and have continued to use it for new systems we build."

    + Dropbox

      * https://dropbox.tech/infrastructure/rewriting-the-heart-of-our-sync-engine
      * https://lobste.rs/s/ob6a8z/rewriting_heart_our_sync_engine

    + Basho

     * [Riak](https://speakerdeck.com/jtuple/hansei-property-based-development-of-concurrent-systems)
       (a distributed NoSQL key-value data store that offers high availability, fault
       tolerance, operational simplicity, and scalability)

    + IOHK

From their recent
[paper](http://www.cse.chalmers.se/~rjmh/tfp/proceedings/TFP_2020_paper_11.pdf)
"Flexibility with Formality: Practical Experience with Agile Formal
Methods in Large-Scale Functional Programming" (2020):

 "Both the network and consensus layers must make significant use of
 concurrency which is notoriously hard to get right and to test. We
 use Software Transactional Memory(STM) to manage the internal state
 of a node. While STM makes it much easier to write correct concurrent
 code, it is of course still possible to get wrong, which leads to
 intermittent failures that are hard to reproduce and debug.

 In order to reliably test our code for such concurrency bugs,
 we wrote a simulator that can execute the concurrent code with
 both timing determinism and giving global observability, producing
 execution traces. This enables us to write property tests that can
 use the execution traces and to run the tests in a deterministic
 way so that any failures are always reproducible.  The use of the
 mini-protocol design pattern, the encoding of protocol interactions
 in session types and the use of a timing reproducable simulation has
 yielded several advantages:

   * Adding new protocols (for new functionality) with strong
     assurance that they will not interact adversly with existing
     functionality and/or performance consistency.

   * Consistent approaches (re-usable design approaches) to issues
     of latency hiding, intra mini-protocol flow control and
     timeouts / progress criteria.

   * Performance consistent protocol layer abstraction /
     subsitution: construct real world realistic timing for operation
     without complexity of simulating all the underlying layer protocol
     complexity. This helps designs / development to maintain performance
     target awareness during development.

   * Consitent error propagation and mitigation (mini protocols to
     a peer live/die together) removing issues of resource lifetime
     management away from mini-protocol designers / implementors."

The simulation code is open source and can be found
[here](https://github.com/input-output-hk/ouroboros-network/tree/master/io-sim).
