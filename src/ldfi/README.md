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
[here](https://github.com/symbiont-io/detsys-testkit/blob/main/src/ldfi/src/Ldfi/Estimate.hs#L156)
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

#### How LDFI cuts down the fault space

Because lineage-driven fault injection uses information from previous runs it is
difficult to estimate the explored fault space, since it is dependent on the
actual system under test. So we will explore this using a specific example that
is also found in the LDFI paper (section 3.1.2).

The example is retrying broadcast, and it has three nodes `A,B,C.` Node `A` has
a special message that it will (repeatably) try to send to all other nodes. The
success criteria is that after the run, node `B` should have the message. Since
`A` keeps sending the message, the only way to make this system fail is by
allowing crashes. But to not get the trivial case of crashing A the first thing,
we put another criteria, that a crashed node was successful in sending a message
before being crashed.

The failurespec that we will use is `EOT=4, EFF=3,Crashes=1`, so let us first
just calculate the total fault space using the estimation above. That gives us
`1830912` combinations of possible faults. But a lot of these combinations
contain faults that will never do anything (Since B,C never sends any messages).

So if we restrict to only making faults that appeared in a run? So we can see
that we can only ever pick a crash for node `A`, and only after time 1. Since we
can only crash a node after they successfully sent a message, and since `B` and
`C` never sends they can't be crashed.

  * No crash -> There are (2^2)^3=64 possible combinations of omissions
  * crash t=2 -> There are (2^2)^1 - 1 = 3 possible combinations of omissions
    ({}, {A->B}, {A->C})
  * crash t=3 -> There are (2^2)^2 - 1 = 15 possible combinations of omissions
  * crash t=4 -> There are (2^2)^3 - 1 = 63 possible combinations of omissions

This are all possible which is only `145` combinations. But this is not all LDFI
does. Suppose the first run introduces an omission for `A -> C` at 1, but we see
that this continued to work (and didn't change the remaining network messages)
so LDFI concludes that this message will not affect the validity of the run
either way. Therefore it is unnecessary to try using this omission anymore. So
the remaining fault space is:

  * No crash -> 2*4*4=32
  * crash t=2 -> 2
  * crash t=3 -> 2*4=8
  * crash t=4 -> 2*4*4=32

So instead of having `145-1=144` more combinations to try, we only have
`32+2+8+32=74` which is almost half the space we had before.

This all seems nice, but this example is a bit deceptively simple, all messages
sent are independent on all other messages. In general a node will send a
message depending on not just the message it just received, but also historic
ones. So if in one run it had received a message before, but in the next one it
didn't it might send a different message. So the space of faults grew (not the
the total one we calculated in the section before, only the one based on
history). This is what makes it really tricky to calculate in general what would
happen.

##### Estimated number of runs

We now have the size of the fault space, but how many runs would it take to find
a real issue? In order to figure that out, we need to know how many of the
possible faults would find an issue. Given such a probability `p` we can
approximate the expected number of runs as `1/p` (this approximation uses the
binomial distribution, and as such has replacement, i.e. the same faults could
be picked multiple times, if we want something better we should use
hyper-geometric distribution but that is more complicated.) So how many faults
would reveal the issue for the broadcasting example? Calculemus!

The only way to get the issue, is if we crash A, after time 1, and in all times
before the crash we make omissions from A to B. We also have to be careful to
make sure one message from A reaches C at some point before the crash. So the
cases are:

  * crash t=2 -> 1 (we can't do omission to both)
  * crash t=3 -> 2^2-1=3
  * crash t=4 -> 2^3-1=7

So there are 11 ways for A, giving the total `11*64^2=45056` possible out of the
total 1830912. This gives us `p=45056/1830912=0.025` which means we can expect
`1/p=40.6` runs before finding the issue if we do completely random search. But
if we have access to the history we instead have `p=11/145=0.076` which has
expected number of runs being `13.2`.
