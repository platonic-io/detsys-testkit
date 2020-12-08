## detsys-testkit

A test kit for fast and deterministic system tests.

System tests are usually slow, non-deterministic and should therefor be used
sparsely -- this project tries to turn this idea on its head.

By abstracting out the non-deterministic parts of the programs we write, e.g.
concurrency, and putting them behind interfaces we can then implement these
interfaces twice:

1. using the non-determinstic primitives, e.g. send and receive over the
   network, this gives you back the original program which you had before
   introducing the abstraction;
2. using functions that determinstically manipulate a pure datastructure, e.g.
   queues representing incoming network messages, this gives you a *simulation*
   of what your original program will do once you use the non-deterministic
   interface.

This simulation forms the basis for our system tests. The two key aspects of
this simulation is that it's determinstic (that doesn't mean there's no
randomness) and that we can control the passage of time (which in turn means we
don't have to wait for timeouts etc), which means we can get fast and
determinstic tests.

For more about simulation testing see [this](doc/simulation_testing.md)
document.

### Getting started

There are many parts that needs to be understood before we can explain how
simulation testing works in detail. It's perhaps easier to start with a concrete
example. The following steps gets you started using some of the tools on a
simple distributed register example:

0. Install [`nix`](https://nixos.org/download.html#nix-quick-install);
1. Clone this repo;
2. `cd` into the repo;
3. Install all development tools and compile all components with `nix-shell`
   (this can take a while the first time you do it);
4. Prepare the database with `detsys db up`;
5. Start the Scheduler with `detsys scheduler up`;
6. Generate a test case with `detsys generate`;
7. `cd src/sut`
8. `go test -run TestRegister1`
9. `detsys debug 1 0` XXX: fix so it's 0 0?
10. Exit debugger with `q` or `Ctrl-c`.

TODO: have a look at the example

More about `nix` can be found [here](doc/nix.md), including why it's preferbable
to `docker` and how to overcome some weak points of `nix-shell`.

### How it works on a higher-level

TODO: Now that we looked at a concrete example...

* TODO: link to video presentation?

### More examples

* Reliable broadcast [example](TODO) from the *Lineage-driven fault injection*
  [paper](https://dl.acm.org/doi/10.1145/2723372.2723711) (2015) by Peter Alvaro
  et al.

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

* The [P](https://github.com/p-org/P) programming language.

### License

See the file [LICENSE](LICENSE).
