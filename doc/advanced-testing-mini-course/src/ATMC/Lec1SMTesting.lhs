> module ATMC.Lec1SMTesting where

> import Control.Monad.IO.Class
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

Plan
----

XXX: ...


SUT
---

> newtype Counter = Counter (IORef Int)

> newCounter :: IO Counter
> newCounter = do
>   ref <- newIORef 0
>   return (Counter ref)

> incr :: Counter -> IO ()
> incr (Counter ref) = do
>   n <- readIORef ref
>   writeIORef ref (n + 1)

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

> genCommand :: Gen Command
> genCommand = elements [Incr, Get]

> genProgram :: Model -> Gen Program
> genProgram _m = Program <$> listOf genCommand

> samplePrograms :: IO [Program]
> samplePrograms = sample' (genProgram initModel)

> validProgram :: Model -> [Command] -> Bool
> validProgram _mode _cmds = True

> shrinkCommand :: Command -> [Command]
> shrinkCommand _cmd = []

> shrinkProgram :: Program -> [Program]
> shrinkProgram _prog = [] -- Exercises.

> forallPrograms :: (Program -> Property) -> Property
> forallPrograms p =
>   forAllShrink (genProgram initModel) shrinkProgram p

> prop_counter :: Property
> prop_counter = forallPrograms $ \prog -> monadicIO $ do
>   c <- run newCounter
>   let m = initModel
>   runProgram c m prog

> runProgram :: MonadIO m => Counter -> Model -> Program -> m Bool
> runProgram c0 m0 (Program cmds) = go c0 m0 cmds
>   where
>      go _c _m []           = return True
>      go  c  m (cmd : cmds) = do
>        resp <- liftIO (exec c cmd)
>        let (m', resp') = step m cmd
>        if resp == resp'
>        then go c m' cmds
>        else return False

Regression tests
----------------

> assertProgram :: String -> Program -> Assertion
> assertProgram msg prog = do
>   c <- newCounter
>   let m = initModel
>   b <- runProgram c m prog
>   assertBool msg b

Discussion
----------

- Why state machines over other forms of specifications? E.g. unit test-suite.

  + First of all, a bunch of unit tests are not a specification in the same way
    that a bunch of examples in math are not a proposition/theorem.

  + Stateless (or pure) property-based testing tries to *approximate* proof by
    induction in math. For example the following is the proposition that
    addition is associative for integers, *forall i j k. (i + j) + k == i + (j +
    k)*. It looks almost exactly like the property you'd write in a
    property-based test, but of course this test passing isn't a proof of the
    proposition, still a step in the right direction if we want to be serious
    about program correctness.

  + XXX: Stateful property-based testing using state machines, like we seen in
    this lecture, tries to approximate proof by structural induction on the
    sequence of inputs. Or inductive invarint method?!

  + Executable (as the REPL exercise shows, but also more on this later)

  + Same state machine specification can be used for concurrent testing (Lec 2)
  + Mental model

  + Already heavily used in distributed systems (later we'll see how the model
    becomes the implementation)

Excerises
---------

0. If you're not comfortable with Haskell, port the above code to your favorite
   programming language.

1. Add a `Reset` `Command` which resets the counter to its initial value.

2. Implement shrinking for programs.

3. Write a REPL for the state machine. Start with the initial state, prompt the
   user for a command, apply the provided command to the step function and
   display the response as well as the new state, rinse and repeat.

   (For a SUT as simple as a counter this doesn't make much sense, but when the
   SUT get more complicated it might make sense to develope the state machine
   specification first, demo it using something like a REPL or some other simple
   UI before even starting to implement the real thing.)

4. Collect timing information about how long each command takes to execute on
   average.

See also
--------

- The original QuickCheck
  [paper](https://dl.acm.org/doi/pdf/10.1145/357766.351266) by Koen Claessen and
  John Hughes (2000) that introduced property-based testing in Haskell.

- John Hughes' Midlands Graduate School 2019
  [course](http://www.cse.chalmers.se/~rjmh/MGS2019/) on property-based testing,
  which covers the basics of state machine modelling and testing. It also
  contains a minimal implementation of a state machine testing library built on
  top of Haskell's QuickCheck;

- Lamport's [Computation and State
  Machines](https://www.microsoft.com/en-us/research/publication/computation-state-machines/)
  (2008)

- "Can one generalize Turing machines so that any algorithm, never mind how ab-
  stract, can be modeled by a generalized machine very closely and faithfully?"

  Perhaps somewhat surprisingly it turns out that the answer is yes, and the
  generalisation is a state machine! (This means that in some sense the state
  machine is the ultimate model?!)

  For details see Gurevich's
  [generalisation](http://delta-apache-vm.cs.tau.ac.il/~nachumd/models/gurevich.pdf)
  of the Church-Turing thesis.
