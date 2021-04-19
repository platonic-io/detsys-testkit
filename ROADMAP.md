### Roadmap

In the issue tracker we currently have five milestones, each explained in its
own section below.

#### v0.0.2

A first version that is self-contained, documented and could be handed over to
someone who could with some workarounds apply it to their problem.

For a detailed list of tickets click
[here](https://github.com/symbiont-io/detsys-testkit/milestone/1).

#### v0.1.0

Clean up tech debt that has accumulated while trying to get the first version
out the door. These refactors also pave the way for future improvements.

[Tickets](https://github.com/symbiont-io/detsys-testkit/milestone/2)

#### v0.2.0

Fix the limitations that require workarounds. This will also make it easier for
us to test parts of the project using the project, which in turn will accelerate
development.

[Tickets](https://github.com/symbiont-io/detsys-testkit/milestone/3)

#### v0.3.0

Optimise the fault-space exploration given current fault support. It's important
that we efficiently estimate and explore the state-space before making it
bigger.

[Tickets](https://github.com/symbiont-io/detsys-testkit/milestone/4)

#### v0.4.0

Add support for more faults.

[Tickets](https://github.com/symbiont-io/detsys-testkit/milestone/5)

#### Future

* Sharable test database/cache, never rerun the same test again (between
  developers and CI);

* Tests that can be resumed (simulate long-running tests but without actually
  having to continuously run the system);

* Make it possible to write tests that involve multiple different versions and
  upgrades between them;

* Load test using an operational profile.

The above items don't have tickets yet.
