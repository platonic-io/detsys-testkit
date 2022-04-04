> module ATMC.Lec2ConcurrentSMTesting where

> import Test.QuickCheck

Concurrent state machine testing with linearisability
=====================================================

Motivation
----------

  - In the previous chapter we saw how to test if a sequential (single-threaded)
    program respects some state machine specification

  - Next we show how the *same* specification can be used to check if a
    concurrent execution is correct using linearisability

  - E.g. counters are often shared among different threads, how can we test that
    the counter implementation is thread-safe?

> data Command = C
> data Model = M
> data Op = O
> type Precondition = Command -> Bool

> newtype Program = Program [Command]

> newtype ConcProgram = ConcProgram [[Command]]

> genConcProgram :: Model -> Gen ConcProgram
> genConcProgram = undefined

> validConcProgram :: Model -> Precondition -> ConcProgram -> Bool
> validConcProgram = undefined

> data History = History Op

> execConc :: ConcProgram -> IO History
> execConc = undefined

> linearisable :: History -> Bool
> linearisable = undefined
