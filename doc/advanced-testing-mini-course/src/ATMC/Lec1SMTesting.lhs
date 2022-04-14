> module ATMC.Lec1SMTesting where

> import Data.IORef
> import Test.QuickCheck
> import Test.QuickCheck.Monadic
> import Test.HUnit

State machine testing
=====================

Motivation
----------

  - Testing: "the process of using or trying something to see if it works, is
    suitable, obeys the rules, etc." -- Cambridge dictionary

  - In order to check that the software under test (SUT) obeys the rules we must
    first write down the rules

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

State machine model/specification/fake
--------------------------------------

> newtype FakeCounter = FakeCounter Int

> fakeIncr :: FakeCounter -> (FakeCounter, ())
> fakeIncr (FakeCounter i) = (FakeCounter (i + 1), ())

> fakeGet :: FakeCounter -> (FakeCounter, Int)
> fakeGet (FakeCounter i) = (FakeCounter i, i)


> data Command = Incr | Get
>   deriving (Eq, Show)

> data Response = Unit () | Int Int
>   deriving (Eq, Show)

> type Model = FakeCounter

> initModel :: Model
> initModel = FakeCounter 0

> step :: Model -> Command -> (Model, Response)
> step m cmd = case cmd of
>   Incr -> Unit <$> fakeIncr m
>   Get  -> Int  <$> fakeGet m

> exec :: Counter -> Command -> IO Response
> exec c cmd = case cmd of
>   Incr -> Unit <$> incr c
>   Get  -> Int  <$> get c

> newtype Program = Program [Command]
>   deriving Show

> genProgram :: Model -> Gen Program
> genProgram = undefined

> prop_counter :: Property
> prop_counter = forAll (genProgram initModel) $ \(Program cmds) -> monadicIO $ do
>   c <- run newCounter
>   let m = initModel
>   go c m cmds
>   where
>     go c m []           = return True
>     go c m (cmd : cmds) = do
>       resp <- run (exec c cmd)
>       let (m', resp') = step m cmd
>       if resp == resp'
>       then go c m' cmds
>       else return False

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

0. Add a `Reset` `Command` which resets the counter to its initial value.

1. Implement shrinking for programs.

2. Write a REPL for the state machine. Start with the initial state, prompt the
   user for a command, apply the provided command to the step function and
   display the response as well as the new state, rinse and repeat.

   (For a SUT as simple as a counter this doesn't make much sense, but when the
   SUT get more complicated it might make sense to develope the state machine
   specification first, demo it using something like a REPL or some other simple
   UI before even starting to implement the real thing.)

3. Collect timing information about how long each command takes to execute on
   average.

See also
--------

- Why state machines over other forms of specifications? E.g. unit test-suite.
  + Executable (as the REPL exercise shows, but also more on this later)
  + Mental model
  + Gurevich's generalisation of the Church-Turing thesis
  + Already heavily used in distributed systems (later we'll see how the model
    becomes the implementation)
