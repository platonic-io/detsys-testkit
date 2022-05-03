> module Lec06WhiteboxCheckers where

Motivation
----------

  - Previously we've seen how to black-box test our system, but simulation gives
    us full insight into what's happening inside "the box" so we can write more
    interesting white-box checkers

See also
--------

  - The `detsys` [`ltl`](https://github.com/symbiont-io/detsys-testkit/tree/main/src/ltl) checker
  - [Quickstrom](https://arxiv.org/abs/2203.11532v1)
  - [HasCal](https://github.com/Gabriel439/HasCal)

Exercises
---------

0. Can we model check our state machines a la TLC/HasCal while retaining the
   possibility to simulate and "production deploy" them? (Hint: see the
   [P](https://github.com/p-org/P) programming language.)
