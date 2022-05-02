> module ATMC.Lec07EfficientEventLoop where

Motivation
----------

  - Our event loop is deterministic which allows for simulation, but it's not
    the most efficient in terms of how fast and how much traffic it can serve

Plan
----

  - Pipelining (shared multi-cast queue a la LMAX' disruptor)
  - Journal?


See also
--------

  - The Linux kernel's [io_uring](https://kernel.dk/io_uring.pdf) interface
  - [aeron](https://github.com/real-logic/aeron)
