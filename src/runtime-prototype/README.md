### stunt-double

`stunt-double` is an actor implementation where the real actors can easily and
faithfully be swapped out for fakes, to make testing of distributed systems
easier.

#### Actors

* Erlang is perhaps the most commonly associated programming language when one
  thinks of implementations of actor systems today

* Many other programming languages have tried to copy Erlang, e.g. Scala, Cloud
  Haskell, etc.

* The concept is, of course, older: Smalltalk (1972), Hewitt, Carl; Bishop,
  Peter; Steiger, Richard (1973). "A Universal Modular Actor Formalism for
  Artificial Intelligence". IJCAI.

* Erlang was influenced by Smalltalk, which many people associated with OOP but
  as per Alan Key's
  [email](http://lists.squeakfoundation.org/pipermail/squeak-dev/1998-October/017019.html)
  the key idea is actually message passing.

#### Interfaces (a.k.a. "behaviours")

* Joe Armstrong's PhD
  [thesis](https://erlang.org/download/armstrong_thesis_2003.pdf) "Making
  reliable distributed systems in the presence of software errors" written in
  2003 when he was 53 years old, he started working on telecom systems in 1981.
  Quite different from a typical PhD thesis written by a 20-something year old
  person with no or very little experience from industry...

* Anyway, the thesis can perhaps best be summarised by "the big idea in Erlang
  isn't messaging/actors for concurrency, but using interfaces (a.k.a.
  "behaviours") to *abstract away the concurrency*" (these are not Joe's words,
  but rather my interpretation)

* "network normal form"

* Examples of interfaces: gen_server, gen_supervisor, gen_fsm, gen_event. The
  idea is that an application can be written by largely combining several
  instances of these.

* Concurrent application becomes sequential, easier to write (especially for
  less experience programmers) according to Joe

* easier to maintain, if an interface is improved or a bug fixed in it, then all
  application written against the interface will benefit

* easier to test/prove according to Joe

* In the future work section of the thesis: "How can we program components - the
  idea of a set of communicating processes leads naturally to the idea of
  writing the components in different languages. How can we do this?"

#### Deployments

* Infrastructure is a map from machine id (ip address) to set of Vat ids

* Deployment is a supervisor tree (each actor and supervisor has a child id) and
  a map from child id to machine id and Vat id

* Deploy git commit hashes or hashes of the AST (Unison-style)?

* Join two deployments (make assumptions about existing deployments)?

#### Live debugging

* Connect to (remote) event loop
* REPL
* Check crashes that happened by checking the supervisors' logs
* Check the source code of each actor
* Check the state of each actor

* Keep a ring buffer for each actor with the last X messages it received so we
  can step back and forth through time and see how the state changed

* Keep a ring buffer with the latest Y crashes together with a ring buffer with
  the last Y messages before that crash

#### Upgrades

* Hot-code swapping

* Automatic rollbacks

#### Automatic scaling

* E.g. kubernetes detects high resource usage on some event loop, provisions a new machine, writes to some global register that the new machine is available

#### Protocols

This is an idea that comes from the sessions types community, but instead of
statically checking it at compile-time it's checked during run-time. As far as I
know checking it at run-time instead is due to Joe Armstrong (Erlang).

When an actor communicates via messages with an other then typically some
protocol should be followed.

For example if the protocol is some IRC-like chat protocol and if the first
actor is the client and the second the server, then we'd expect the first
message from the client to be some kind of CONNECT message, followed by picking
a name using some NICK message, followed by joining a channel using JOIN (you
can't join a channel before connecting), once in the channel you can set the
TOPIC (but you can't do that before joining) and so on.

A protocol is actually just another state machine. It keeps track of what
messages have been sent and from there can ensure which messages are allowed or
not.

#### Capabilities

* E programming language
* [Goblins](https://spritelyproject.org/#goblins)
* OpenBSD's [pledge](https://man.openbsd.org/pledge.2) and
  [unveil](https://man.openbsd.org/unveil.2)

#### Deterministic testing

#### See also

* Erlang's [supervisors](https://erlang.org/doc/man/supervisor.html);
* https://capnproto.org/

#### How to run tests

cabal configure test \
    --test-option='--timeout=10' \
    --test-option='--color=always' \
    --test-show-details=streaming \
    --ghc-options='-threaded -rtsopts -with-rtsopts=-N -fno-ignore-asserts' \
    # --test-option='--pattern=/$pattern/
cabal test
