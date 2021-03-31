### Lineage-driven fault injection

An implementation of lineage-driven fault injection as first described in the
following [paper](http://people.ucsc.edu/~palvaro/molly.pdf) by Peter Alvaro et
al (2015).

The main ideas is that instead of randomly injecting faults, we can start with a
successful run of the test and try to figure out which steps crucially supported
the successful outcome and then intentionally try to target those steps in order
to produce a possible failure.

#### Failure specification

A failure specification is defined in order to be able to bound our search and
to estimate how big the possible fault space is.

The failure specification consists of:

  * The amount of nodes involved in the test;
  * The time when the test ends (or EOT for "end of time");
  * The time when the network heals and no more omissions or partitions happen
    (EFF for "end of finite failures");
  * The max amount of node crashes allowed.

The time above is in all cases logical time, so for example if EOT=3 that means
that there are three discrete points in time when communication between nodes
(and therefore omissions) or crashes can happen.

Given a failure specification we can therefor enumerate all possible failures.
For example, lets say we got two nodes A and B, EOT=3 and EFF=2 and max two
crashes are allowed. Here are some of the possible faults that can happen in
that within that failure specification:

  * A crashes at time 1, 2 or 3;
  * B crashes at time 1, 2 or 3;
  * A crashes at time 1 and B crashes at time 1, 2 or 3;
  * A crashes at time 2 and B crashes at time 1, 2 or 3;
  * A crashes at time 3 and B crashes at time 1, 2 or 3;
  * B crashes at time 1 and A crashes at time 1, 2 or 3;
  * B crashes at time 2 and A crashes at time 1, 2 or 3;
  * B crashes at time 3 and A crashes at time 1, 2 or 3;

  * There's an omission between A and B at time 1 or 2 (it can't happened at 3
    because EFF=2);
  * Omission between B and A at time 1 or 2;
  * Omission between A and B at time 1 and 2;
  * Omission between A and B at time 1 and 2, and omission between B and A at time 1 or 2;
  * Omission between A and B at time 1 and 2, and omission between B and A at time 1 and 2;

  * Omission between A and B at time 1 and A crashes at 2.

These are some of the 121 possible failures that can happen. If we take a
successful run and try injecting all of the faults according to some failure
specification and it still produces the correct outcome, then we have certified
that run according to that specific failure specification.

#### How to estimate the fault space

Running 121 test doesn't seem to bad, but this figure grows very fast as we
increase the parameters involved in the failure specification as we shall see
next.

How did we arrive at 121 in the first place? Let's do a naive estimate first,
assuming that crashes and omissions are independent.

Each node has 4 choices of when to crash (never, at time 1, 2, or 3) and 4
possible omissions (no omissions, from the node to the other at time 1, from the
node to the other at time 2, or from the node to the other at time 1 and 2).

There are two nodes, so we get (4*4)^2 = 256 possible faults.

However if we crash a node it doesn't make sense to introduce omissions from it
afterwards, so crashes and omissions aren't independent. If we condition
omissions on crashes we get the following for each node:

  * No crash -> 4 possibilities (the ones listed above);
  * Crash at time 1 -> 1 possibility (no omissions);

  * Crash at time 2 -> 2 possibilities (no omissions or from the node to the
    other at time 1);
  * Crash at time 3 -> 4 possibilities (same as no crash, since EFF=2).

So we get 4 + 1 + 2 + 4 = 11, and since we got two nodes that both might crash
11^2 = 121 possible failures.

Before we give a general formula for estimating the fault space, lets do a
couple of variations of the above example to further solidify our intuition.

Lets change max crashes to 1 and see what happens.

First we need to choose which of the two nodes to crash, there are 2 ways to do
that (2 choose 1). A non-crashing node only has 4 possible failures due to
omissions. So we get 2 * 11 * 4 = 88.

Next lets change the amount of nodes involved to 3.

This changes the possible omissions for each node since there's now an extra
node to communicate with. The possible omissions are now: no omissions, omission
from node A to node B at time 1, omission from node A to node C at time 1,
omission from node A to node B at time 2, etc. We can generate this set by:

  `PowerSet({ omission("A", to, at) | to in {"B", "C"} and at in {1, 2} })`

Or 2 (to) ^2 (at) ^2 (power set) = 16 possible omissions.

Now if condition those on crashes, we get:

  * No crash -> 16;
  * Crash at time 1 -> 1 (no omissions);
  * Crash at time 2 -> 4 (no omissions, omission from A to B at 1, omission from
    A to C at 1, and omission from A to B and C at 1);
  * Crash at time 3 -> 16.

If we add all that together, 16 + 1 + 4 + 16 = 37, we get the possible omissions
of a crashing node.

Given that we got 3 nodes now out which max one will crash, we get: 3 choose 1 *
37 * 16^2 = 28416 possible faults.

The general formula can be found
[here](https://github.com/symbiont-io/detsys-testkit/blob/main/src/ldfi2/src/Ldfi/Estimate.hs#L156)
and if we run it on slightly larger failure specifications the following is what
we get:

| EOT | EFF | Crashes | Nodes |   Combinations|
|:---:|:---:|:-------:|:-----:|--------------:|
|   4 |   2 |       1 |     3 |         40704 |
|   5 |   2 |       1 |     3 |         52992 |
|   5 |   3 |       1 |     3 |       2617344 |
|   5 |   3 |       1 |     4 |  863825297408 |
|   5 |   3 |       2 |     4 | 4071957725184 |
|   6 |   4 |       1 |     5 |    1.85*10^25 |
|   9 |   7 |       1 |     4 |    2.43*10^26 |

Where the last two entries correspond to the minimal failure specification
needed to find a durability violation in Kafka and an agreement violation in 3PC
respectively, see the LDFI paper for details.

It's worth noting that while this number takes into account that no other faults
can happen after a crash it doesn't take into account that for example an
omission between node A and C at time 1 might not "do anything" if in the run of
the program we are testing no communication actually happens between A and C at
that time. Truly random fault injection has no information that can help it
avoid trying injecting that fault, even though it clearly won't make a difference.

This is where lineage-driven fault injection differentiates it from random
search, in that it actually uses the information from previous runs to narrow
down the search space. Hopefully, by now, it should be clear that the space is
so huge that we frankly need techniques like this to achieve any meaningful
coverage.
