State machine testing
=====================

Recap: property-based testing
-----------------------------------

- `forall (xs: List Int). reverse (reverse xs) == xs`
- `forall (i : Input). serialise (deserialise i) == i`
- `forall (i j k : Int). (i + j) + k == i + (j + k)`

Motivation
----------

- The combinatorics of testing stateful systems:
  + $n$ features and 3-4 tests per feature         $\Longrightarrow O(n)$   test cases
  + $n$ features and testing pairs of features     $\Longrightarrow O(n^2)$ test cases
  + $n$ features and testing triples of features   $\Longrightarrow O(n^3)$ test cases
  + Race conditions? (at least two features, non-deterministic)

Plan
----

- Testing: "the process of using or trying something to see if it works, is
  suitable, obeys the rules, etc." -- Cambridge dictionary

- In order to check that the software under test (SUT) obeys the rules we must
  first write down the rules

- A state machine specification is one a way to formally "write down the
  rules"

- Since the state machine specification is executable (we can feed it input and
  get output), we effectively got a [test
  oracle](https://en.wikipedia.org/wiki/Test_oracle) or a [test double
  fake](https://en.wikipedia.org/wiki/Test_double) of the SUT

- Testing strategy: generate a sequence of random inputs, run it against the
  real SUT and against the fake and see if the outputs match

How it works
------------

Test case generation
--------------------

![](./images/generator.svg){ width=400px }


State machine testing
---------------------

![](./images/sm-testing-small.jpg){ width=500px }

Shrinking, when assertions fail
-------------------------------

![](./images/shrinking-small.jpg){ width=400px }

Regression testing
------------------

![](./images/regression.svg){ width=400px }

Coverage
--------

- Risk when generating random test cases: are we generating interesting test cases?
- How to measure coverage
- Corner case thinking and unit tests as basis, e.g. try 0, -1, maxInt, etc

![](./images/coverage.svg){ width=400px }


SUT
---

> module Lec01SMTesting where

> import Control.Monad.IO.Class
> import Data.IORef
> import Test.QuickCheck
> import Test.QuickCheck.Monadic
> import Test.HUnit

The software under test (SUT) of the day is a counter that can be incremented
and read from. It's implemented using a mutable reference (`IORef`) to an `Int`.

> newtype Counter = Counter (IORef Int)

> newCounter :: IO Counter
> newCounter = do
>   ref <- newIORef 0
>   return (Counter ref)

> incr :: Counter -> Int -> IO ()
> incr (Counter ref) i = do
>   j <- readIORef ref
>   if j > 1000
>   then writeIORef ref (i + j + 1) -- NOTE: this is a bug!
>   else writeIORef ref (i + j)

> get :: Counter -> IO Int
> get (Counter ref) = readIORef ref

State machine model/specification/fake
--------------------------------------

The specification of our SUT is state machine model that uses a plain `Int`
(unlike the real implementation it uses no mutable reference).

> newtype FakeCounter = FakeCounter Int
>   deriving Show

A state machine is a function from the current state and some input to the
updated state and some output. We introduce two new types for the input and
outputs:

> data Command = Incr Int | Get
>   deriving (Eq, Show)

> data Response = Unit () | Int Int
>   deriving (Eq, Show)

Next we define the initial state and the state machine function.

> type Model = FakeCounter -- A.k.a. state

> initModel :: Model
> initModel = FakeCounter 0

> step :: Model -> Command -> (Model, Response)
> step m cmd = case cmd of
>   Incr i -> Unit <$> fakeIncr m i
>   Get    -> Int  <$> fakeGet m
>   where
>     fakeIncr :: FakeCounter -> Int -> (FakeCounter, ())
>     fakeIncr (FakeCounter i) j = (FakeCounter (i + j), ())

>     fakeGet :: FakeCounter -> (FakeCounter, Int)
>     fakeGet (FakeCounter i) = (FakeCounter i, i)

Testing library
---------------

Recall that we want generate a random program and then run it against the SUT
and the state machine model and assert that the outputs match.

We want to generate random programs, so lets first define what a program is.

> newtype Program = Program [Command]
>   deriving Show

A program generator can now be defined using combinators provided by the
property-based testing library.

> genProgram :: Model -> Gen Program
> genProgram _m = Program <$> listOf genCommand

> genCommand :: Gen Command
> genCommand = oneof [Incr <$> genInt, return Get]

> genInt :: Gen Int
> genInt = oneof [arbitrary] -- , elements [0, 1, maxBound, -1, minBound]] -- TODO: Fix coverage by uncommenting.

We can sample our program generator to get a feel for what kind of programs it
generates.

> samplePrograms :: IO [Program]
> samplePrograms = sample' (genProgram initModel)

In case we generate a program for which the state machine model and SUT disagree
we'd like to shrink the program before presenting it to the user in order to
make it easier to see what went wrong.

> shrinkProgram :: Program -> [Program]
> shrinkProgram (Program cmds) = [ Program (merge cmds') | cmds' <- shrinkList shrinkCommand cmds ]
>   where
>     merge []                        = []
>     merge (Incr i : Incr j : cmds') = Incr (i + j) : merge cmds'
>     merge (cmd : cmds')             = cmd : merge cmds'

> shrinkCommand :: Command -> [Command]
> shrinkCommand (Incr i) = [ Incr i' | i' <- shrink i ]
> shrinkCommand Get      = []

Finally we have all the pieces necessary to write our property that generates
programs, runs them against the SUT and the model, and asserts that the outputs
are the same.

> prop_counter :: Property
> prop_counter = forallPrograms $ \prog -> monadicIO $ do
>   c <- run newCounter
>   let m = initModel
>   (b, hist) <- runProgram c m prog
>   monitor (coverage hist)
>   return b

> forallPrograms :: (Program -> Property) -> Property
> forallPrograms p =
>   forAllShrink (genProgram initModel) shrinkProgram p

> runProgram :: MonadIO m => Counter -> Model -> Program -> m (Bool, Trace)
> runProgram c0 m0 (Program cmds0) = go c0 m0 [] cmds0
>   where
>      go _c _m hist []           = return (True, reverse hist)
>      go  c  m hist (cmd : cmds) = do
>        resp <- liftIO (exec c cmd)
>        let (m', resp') = step m cmd
>        if resp == resp'
>        then go c m' (Step m cmd resp m' : hist) cmds
>        else return (False, reverse hist)

> exec :: Counter -> Command -> IO Response
> exec c cmd = case cmd of
>   Incr i -> Unit <$> incr c i
>   Get    -> Int  <$> get c

As a biproduct of running our generated program we also produce a trace of which
commands gave what responses and what the state as before and after said command
was executed.

> type Trace = [Step]

> data Step = Step
>   { sModelBefore :: Model
>   , sCommand     :: Command
>   , sResponse    :: Response
>   , sModelAfter  :: Model
>   }

Such traces are useful for many things, for example ensuring that we got good coverage.

> coverage :: Trace -> Property -> Property
> coverage hist = classifyLength hist . classifyOverflow hist
>   where
>     classifyLength xs = classify (length xs == 0)                      "0 length"
>                       . classify (0   < length xs && length xs <= 10)  "1-10 length"
>                       . classify (10  < length xs && length xs <= 50)  "11-50 length"
>                       . classify (50  < length xs && length xs <= 100) "51-100 length"
>                       . classify (100 < length xs && length xs <= 300) "101-300 length"
>                       . classify (300 < length xs && length xs <= 500) "301-500 length"
>     classifyOverflow [] = id

>     classifyOverflow (Step (FakeCounter c) (Incr i) _resp _model' : hist') =
>        classify (isOverflow c i) "overflow" . classifyOverflow hist'
>     classifyOverflow (_ : hist') = classifyOverflow hist'

>     isOverflow i j = toInteger i + toInteger j > toInteger (maxBound :: Int)

Regression tests
----------------

When we find a counterexample that breaks our property we can add a regression
test for it by merely copy-pasting the generated program into our test suite and
using the following function, which does the same thing as our propery, but
skips the generation step.

> assertProgram :: String -> Program -> Assertion
> assertProgram msg prog = do
>   c <- newCounter
>   let m = initModel
>   (b, _hist) <- runProgram c m prog
>   assertBool msg b

Discussion
----------

- Q: The specification is longer than the SUT!?

  A: For something as simple as a counter, this is true, but for any "real
     world" system that e.g. persists to disk the model will likely be smaller
     by an order of magnitude or more. Also the model can also be used for race
     condition testing (lecture 2) and as a fake (lecture 3).

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

4. Add a coverage check ensures that we do a `Get` after an overflow has happened.

5. Write a display function for `Trace` which shows how the system evolved over
   time, the output could for example look like this:

  ```
     state0
       == command0 ==> response0
     state1
       == command1 ==> response1
     ...
  ```

5. Collect timing information about how long each command takes to execute on
   average.

See also
--------

- For more on how feature interaction gives rise to bugs see the following [blog
  post](https://www.hillelwayne.com/post/feature-interaction/) by Hillel Wayne
  summarising [Pamela Zave](https://en.wikipedia.org/wiki/Pamela_Zave)'s work on
  the topic;

- The original QuickCheck
  [paper](https://dl.acm.org/doi/pdf/10.1145/357766.351266) by Koen Claessen and
  John Hughes (2000) that introduced property-based testing in Haskell;

- John Hughes' Midlands Graduate School 2019
  [course](http://www.cse.chalmers.se/~rjmh/MGS2019/) on property-based testing,
  which covers the basics of state machine modelling and testing. It also
  contains a minimal implementation of a state machine testing library built on
  top of Haskell's QuickCheck;

- John Hughes' *Testing the Hard Stuff and Staying Sane*
  [talk](https://www.youtube.com/watch?v=zi0rHwfiX1Q) (2013-2014);

- Lamport's [Computation and State
  Machines](https://www.microsoft.com/en-us/research/publication/computation-state-machines/) (2008)

- "Can one generalize Turing machines so that any algorithm, never mind how ab-
  stract, can be modeled by a generalized machine very closely and faithfully?"

  Perhaps somewhat surprisingly it turns out that the answer is yes, and the
  generalisation is a state machine! (This means that in some sense the state
  machine is the ultimate model?!)

  For details see Gurevich's
  [generalisation](http://delta-apache-vm.cs.tau.ac.il/~nachumd/models/gurevich.pdf)
  of the Church-Turing thesis.

Summary
-------

Property-based testing lets us *generate unit tests* for pure
functions/components, property-based testing using state machine models lets us
generate unit tests for *stateful* functions/components.
