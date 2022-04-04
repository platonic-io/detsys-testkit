# Towards fast and deterministic system tests

## What are system tests?

* A system test involves a (sub)system together with it's environment. The
  environment consists of everything that is out of the systems control, e.g.
  users, faults, other subsystems, etc.

* For example
    - Jepsen tests
        + The subsystem under test here is a database cluster, faults are
          injected, user traffic randomly generated;
    - Performance tests
        + Very demanding or many users

## The consequences of slow and non-deterministic system tests

* Low test coverage where it matters the most (global correctness does not
  follow from local correctness. It's possible for every part to be individually
  rock-solid but the system to be broken as a whole);

* Impossible to write regession tests for complex failures, e.g. the failure
  only occurs if some exact steps happen (due to non-determinism the test will
  only pass or fail sometimes);

* Flaky tests together with all waste of time, resources and context switching
  associated with them.

## The general recipe for fast and deterministic system tests

* Program against interfaces
* Implement the interfaces twice
    1. Actually doing what they are supposed to
    2. Manipulating some in-memory representation of the actual thing
* Example: filesystem interaction
    - Interface could be something like `writeFile : FilePath -> String -> ()`
      and `readFile : FilePath -> String`
    - The real implementation actually writes and reads file from the disk
    - The mock/fake implementation writes and reads from an in-memory map from
      `FilePath` to `String`
* If all your program calls that could be non-deterministic, e.g. filesystem
  interaction, networking, random number generation, timestamps, are all behind
  interfaces and can be swapped in for deterministic mocks/fakes then you can
  start writing determininistic system tests
* Note that you can still introduce non-determinsim, for example in networking
  scheduling, by parametrising the interface by a seed which can then be used to
  determinstically generate pseudo randomness
* Also notice that fault injection is much easier on a mock/fake than on the
  real thing, e.g. try writing a test that involves a full disk.

## Where do these ideas come from?

The principles described above can be found in the following works under the
common keyword "simulation".

### NATO's Software Engineering conference in 1968

Here's the concluding remark from Alan Perlis (the first recipient of the Turing
Award) about a discussion about a paper on simulation testing by Brian Randell
at the first
[conference](http://homepages.cs.ncl.ac.uk/brian.randell/NATO/nato1968.PDF) on
Software Engineering (this is where we got the term from) in 1968:

> I’d like to read three sentences to close this issue.
>
>   1. A software system can best be designed if the testing is interlaced with
>      the designing instead of being used after the design.
>
>   2. A simulation which matches the requirements contains the control which
>      organizes the design of the system.
>
>   3. Through successive repetitions of this process of interlaced testing and
>      design the model ultimately becomes the software system itself. I think that it
>      is the key of the approach that has been suggested, that there is no such
>      question as testing things after the fact with simulation models, but that in
>      effect the testing and the replacement of simulations with modules that are
>      deeper and more detailed goes on with the simulation model controlling, as it
>      were, the place and order in which these things are done.

### FoundationDB

* Explained [here](https://apple.github.io/foundationdb/testing.html) in text
  form;

* Will Wilson also gave a [talk](https://www.youtube.com/watch?v=4fFDFbi3toc)
  about it at Strange Loop 2014;

* By combining simulation testing with fault injection we can test properties
  such as linearisability, but much faster than the black-box approach of
  Jepsen. In fact the FoundationDB people pushed this idea so far that Kyle
  "aphyr" Kingsbury (the main guy behind Jepsen)
  [said](https://twitter.com/aphyr/status/405017101804396546 ) it wasn't worth
  writing Jepsen tests for FoundationDB:

      "haven't tested foundation in part because their testing appears to be
      waaaay more rigorous than mine."

### AWS

Here's a quote from the recently published paper [Millions of Tiny
Databases](https://www.usenix.org/conference/nsdi20/presentation/brooker) (2020)
written by three AWS engineers:

> To solve this problem [testing distributed systems], we picked an approach that
> is in wide use at Amazon Web Services, which we would like to see broadly
> adopted: build a test harness which abstracts networking, performance, and
> other systems concepts (we call it a simworld). The goal of this approach is to
> allow developers to write distributed systems tests, including tests that
> simulate packet loss, server failures, corruption, and other failure cases, as
> unit tests in the same language as the system itself. In this case, these unit
> tests run inside the developer’s IDE (or with junit at build time), with no need
> for test clusters or other infrastructure. A typical test which tests
> correctness under packet loss can be implemented in less than 10 lines of Java
> code, and executes in less than 100ms. The Physalia team have written hundreds
> of such tests, far exceeding the coverage that would be practical in any
> cluster-based or container-based approach.
>
> The key to building a simworld is to build code against abstract physical layers
> (such as networks, clocks, and disks). In Java we simply wrap these thin layers
> in interfaces. In production, the code runs against implementations that use
> real TCP/IP, DNS and other infrastructure. In the simworld, the implementations
> are based on in-memory implementa- tions that can be trivially created and torn
> down. In turn, these in-memory implementations include rich fault-injection
> APIs, which allow test implementors to specify simple statements like:
> `net.partitionOff ( PARTITION_NAME , p5.getLocalAddress () ); ...
> net.healPartition ( PARTITION_NAME );`
>
> Our implementation allows control down to the packet level, allowing testers
> to delay, duplicate or drop packets based on matching criteria. Similar
> capabilities are available to test disk IO. Perhaps the most important testing
> capability in a distributed database is time, where the framework allows each
> actor to have it’s own view of time arbitrarily controlled by the test.
> Simworld tests can even add Byzantine conditions like data corruption, and
> operational properties like high la- tency. We highly recommend this testing
> approach, and have continued to use it for new systems we build.

### Dropbox

* https://dropbox.tech/infrastructure/rewriting-the-heart-of-our-sync-engine
* https://lobste.rs/s/ob6a8z/rewriting_heart_our_sync_engine

### Basho

* [Riak](https://speakerdeck.com/jtuple/hansei-property-based-development-of-concurrent-systems)
  (a distributed NoSQL key-value data store that offers high availability, fault
  tolerance, operational simplicity, and scalability)

### IOHK

From their recent
[paper](http://www.cse.chalmers.se/~rjmh/tfp/proceedings/TFP_2020_paper_11.pdf)
"Flexibility with Formality: Practical Experience with Agile Formal
Methods in Large-Scale Functional Programming" (2020):

> Both the network and consensus layers must make significant use of
> concurrency which is notoriously hard to get right and to test. We
> use Software Transactional Memory(STM) to manage the internal state
> of a node. While STM makes it much easier to write correct concurrent
> code, it is of course still possible to get wrong, which leads to
> intermittent failures that are hard to reproduce and debug.
>
> In order to reliably test our code for such concurrency bugs,
> we wrote a simulator that can execute the concurrent code with
> both timing determinism and giving global observability, producing
> execution traces. This enables us to write property tests that can
> use the execution traces and to run the tests in a deterministic
> way so that any failures are always reproducible.  The use of the
> mini-protocol design pattern, the encoding of protocol interactions
> in session types and the use of a timing reproducable simulation has
> yielded several advantages:
>
>   * Adding new protocols (for new functionality) with strong
>     assurance that they will not interact adversly with existing
>     functionality and/or performance consistency.
>
>   * Consistent approaches (re-usable design approaches) to issues
>     of latency hiding, intra mini-protocol flow control and
>     timeouts / progress criteria.
>
>   * Performance consistent protocol layer abstraction /
>     subsitution: construct real world realistic timing for operation
>     without complexity of simulating all the underlying layer protocol
>     complexity. This helps designs / development to maintain performance
>     target awareness during development.
>
>   * Consitent error propagation and mitigation (mini protocols to
>     a peer live/die together) removing issues of resource lifetime
>     management away from mini-protocol designers / implementors.

The simulation code is open source and can be found
[here](https://github.com/input-output-hk/ouroboros-network/tree/master/io-sim).

## Further reading

Since this kind of testing is still cutting edge, there are still
very few examples of it available for study. AWS and Dropbox are
obviously closed source, and the gritty details are omitted from the
above papers. The FoundationDB code is [open
source](https://github.com/apple/foundationdb). However
even running the tests is
[underdocumented](https://github.com/apple/foundationdb/issues/2368),
so unless you are willing to learn their custom made C++ actor
library and read their C++ code it's difficult to learn much from it.

The bits and pieces that I've found most insightful are various blog
posts and comments. Here's a list of them together with a short
description of the given insight:

  * Recipe for building a simulator, including the network
    [interface](https://lobste.rs/s/igiolo/learning_build_distributed_systems#c_nlpl7r);
  * How to design the system to make it easier to simulation
    [test](https://lobste.rs/s/ob6a8z/rewriting_heart_our_sync_engine#c_ab2ysi);
  * One way of how to do deterministic scheduling of
    [threads](https://lobste.rs/s/ob6a8z/rewriting_heart_our_sync_engine#c_6o7uvy);
  * Testing error handling code in a deterministic
    [way](http://sled.rs/errors);
  * Non-brute force way of exploring the intereaving+fault
    [space](https://lobste.rs/s/ob6a8z/rewriting_heart_our_sync_engine#c_nelcao);
  * Summary of including some of the [above](http://sled.rs/simulation).

The closest thing to useful readable code that I've been able to find
is the following
[tutorial](https://github.com/spacejam/quickcheck-tut), it consists
of three examples in increasing complexity. The `tree_model` example is
an introduction to model-based testing using a property based testing
library where the software under test is a tree-like datastructure. In the
second example, `election_simulator` the software under test is a
simple consensus protocol, and the last example `caspaxos` is a more
complicated consensus protocol. In both consensus examples networking
and time is done deterministically.

There's also the more established practice of [discrete-event
simulation](https://en.wikipedia.org/wiki/Discrete-event_simulation)
which is usually used in different contexts than software testing,
but nevertheless is close enough in principle that it's worth taking
inspiration from (and indeed the simulation testing people often
refer to it). Here are a few resources:

  * [Introduction to Modeling and
    Simulation](https://john.cs.olemiss.edu/~hcc/csci405/00spr/notes/introModelSim.html)
    and [An Introduction to Discrete-Event
    Simulation](https://www.cs.cmu.edu/~music/cmsip/readings/intro-discrete-event-sim.html);

  * [Writing a Discrete Event Simulation: ten easy
    lessons](https://users.cs.northwestern.edu/~agupta/_projects/networking/QueueSimulation/mm1.html);

  * [Discrete-Event Simulation lecture notes by Graham
    Horton](http://isgwww.cs.uni-magdeburg.de/~graham/its_01/lectures/04-DESimulation-1.pdf).
    Part of a
    [course](http://isgwww.cs.uni-magdeburg.de/~graham/its_01/)
    on simulation;

  * https://ocw.mit.edu/courses/mechanical-engineering/2-875-mechanical-assembly-and-its-role-in-product-development-fall-2004/lecture-notes/cls20_smltion04.pdf

  * https://ocw.mit.edu/courses/mechanical-engineering/2-854-introduction-to-manufacturing-systems-fall-2016/lecture-notes/MIT2_854F16_Simulation.pdf

  * https://www.grotto-networking.com/DiscreteEventPython.html

  * [Discrete Event Simulation: Implementation and
    Test](https://www.coursera.org/lecture/progfun2/lecture-4-6-discrete-event-simulation-implementation-and-test-62Xed)
    video lecture by Martin Odersky.
  * [Comment and link to longer note on how time works in
    discrete-event simulations](https://news.ycombinator.com/item?id=23295059)

There are also many books written on (discrete-event) simulation, e.g.:

  * *A Guide to Simulation* by Paul Bratley, Bennet L. Fox and Linus E. Schrage
    (2nd ed, 1987)

  * *Discrete-Event Simulation: A First Course* by Lawrence M. Leemis and
    Stephen K. Park (1st ed, 2004/2006?)
    [(PDF)](http://www.coins-lab.org/imamu/akoubaa/cs433/Textbooks/Discrete%20Event%20Simulation%20-%20A%20First%20Course%20-%20Lemmis%20Park.pdf)

  * *Discrete-Event System Simulation* by Jerry Banks, John S. Carson
    II, Barry L. Nelson and David M. Nicol
