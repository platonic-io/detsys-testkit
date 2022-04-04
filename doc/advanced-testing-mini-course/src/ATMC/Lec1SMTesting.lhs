> module ATMC.Lec1SMTesting where

> import Data.IORef
> import Test.QuickCheck
> import Test.HUnit

State machine testing
=====================

Motivation
----------

  - Testing: "the process of using or trying something to see if it works, is
    suitable, obeys the rules, etc." -- Cambridge dictionary

  - In order to check that the software under test (SUT) obeys the rules we must
    first write the rules down

  - State machine specifications are one of many ways to formally "write down
    the rules"

SUT
---

> newtype Counter = Counter (IORef Int)

> newCounter :: IO Counter
> newCounter = do
>   ref <- newIORef 0
>   return (Counter ref)

> incr :: Counter -> IO ()
> incr (Counter ref) = modifyIORef ref (+ 1)

> get :: Counter -> IO Int
> get (Counter ref) = readIORef ref

Model
-----

> newtype FakeCounter = FakeCounter Int

> fakeIncr :: FakeCounter -> (FakeCounter, ())
> fakeIncr (FakeCounter i) = (FakeCounter (i + 1), ())

> fakeGet :: FakeCounter -> (FakeCounter, Int)
> fakeGet (FakeCounter i) = (FakeCounter i, i)


> data Command = Incr | Get
>   deriving Show

> data Response = Unit () | Int Int

> type Model = FakeCounter

> initModel :: Model
> initModel = FakeCounter 0

> step :: Model -> Command -> (Model, Response)
> step m cmd = case cmd of
>   Incr -> Unit <$> fakeIncr m
>   Get  -> Int  <$> fakeGet m

> exec :: Counter -> Command -> IO Response
> exec = undefined

> newtype Program = Program [Command]
>   deriving Show

> genProgram :: Model -> Gen Program
> genProgram = undefined

> prop_counter :: Property
> prop_counter = forAll (genProgram initModel) $ \(Program cmds) -> do
>   True

Regression tests
----------------

> runProgram :: Program -> IO Bool
> runProgram = undefined

> assertProgram :: String -> Program -> Assertion
> assertProgram msg prog = do
>   b <- runProgram prog
>   assertBool msg b

Excerises
---------

1. Implement shrinking for programs.

2. Collect timing information about how long each command takes to execute on
   average.

See also
--------

- Why state machines over other forms of specifications?
  + Executable (we will use this later)
  + Gurevich's generalisation of the Church-Turing thesis
  + Already heavily used in distributed systems
