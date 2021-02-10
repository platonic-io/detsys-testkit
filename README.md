## detsys-testkit

System tests are usually slow, non-deterministic and should therefor be used
sparsely -- this project tries to turn this idea on its head.

By abstracting out the non-deterministic parts of the programs we write, e.g.
concurrency, and putting them behind interfaces we can then implement these
interfaces twice:

1. using the non-deterministic primitives, e.g. send and receive over the
   network, this gives you back the original program which you had before
   introducing the abstraction;
2. using functions that deterministically manipulate a pure data-structure, e.g.
   queues representing incoming network messages, this gives you a *simulation*
   of what your original program will do once you use the non-deterministic
   interface.

This simulation forms the basis for our system tests. The two key aspects of
this simulation is that it's deterministic (that doesn't mean there's no
randomness) and that we can control the passage of time (which in turn means we
don't have to wait for timeouts etc), which means we can get fast and
deterministic tests.

For more about simulation testing see [this](doc/simulation_testing.md)
document.

### Getting started

There are many parts that needs to be understood before we can explain how
simulation testing works in detail. It's perhaps easier to start with a concrete
example. The following steps gets you started using some of the tools on a
simple distributed register example:

0. Install [`nix`](https://nixos.org/download.html#nix-quick-install);
1. Clone this repository;
2. `cd` into the repository;
3. Install all development tools and compile all components with `nix-shell`
   (this can take a while the first time you do it);
4. Prepare the database with `detsys db up`;
5. Start the scheduler component with `detsys scheduler up`;
6. Change directory to where the distributed register example lives with `cd
   src/sut/register`;
7. Run the first test from the test-suite by typing `go test -run 1`;
8. Notice how test id `1` and run id `2` doesn't pass the analysis. To debug
   that run enter `detsys debug 1 2`;
9. Navigate up and down through the messages with your arrow keys or `j` and
   `k`, and finally exit the debugger with `q` or `Ctrl-c`.

At this point it might make sense to have a look at the `go` test-suite in
[`example_test.go`](src/sut/register/example_test.go) and the actual
implementation of the distributed register. The implementation consists of two
parts: a front-end (see [`frontend1.go`](src/sut/register/frontend1.go)) which
receives client requests and propagates them to two distributed registers
([`register.go`](src/sut/register/register.go)). The client can be request a
write to or a read from the registers, and the idea with there being two
registers is that there is some form of redundancy. The implementation is flawed
however, as you might have been able to guess from the fact that the test fails.
Can you figure out what exactly the implementation does and why it's wrong by
using the debugger alone? For further refinements of the implementation see
[`frontend{2,3,4}.go`](src/sut/register) and see if you can figure out why they
do or don't work as well.

More about `nix` can be found [here](doc/nix.md), including a recommended
developer workflow and why it's preferable to `docker`.

### How it works on a higher-level

TODO: Now that we looked at a concrete example...

* TODO: link to video presentation?

### More examples

* Reliable broadcast [example](src/sut/broadcast) from the *Lineage-driven fault
  injection* [paper](https://dl.acm.org/doi/10.1145/2723372.2723711) (2015) by
  Peter Alvaro et al.

### How it works on a lower-level

* How each component works (see their respective `README.md`);
* Database schema;
* Interfaces between components (APIs):
    * Scheduler (see also pseudo
      [code](doc/pseudo_code_for_discrete-event_simulator.md) for discrete-event
      simulation);
    * Executor;
    * SUT (see also [network normal form](doc/network_normal_form.md).

### How to contribute

See the file [CONTRIBUTING.md](CONTRIBUTING.md).

### See also

* The [P](https://github.com/p-org/P) programming language;
* [Simulant](https://github.com/Datomic/simulant);
* [Jepsen](https://github.com/jepsen-io/jepsen) and
  [Maelstrom](https://github.com/jepsen-io/maelstrom).

### License

See the file [LICENSE](LICENSE).
