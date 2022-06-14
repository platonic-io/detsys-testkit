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

Plan
----

- Reuse the counter SUT and model from previous lecture;

- Generate concurrent programs by instead of generating list of commands
  generate lists of lists of commands where the outer list represents commands
  that should be executed concurrently;

- Collect a concurrent history of when each command started and finished
  executing on each thread;

- Try to find a sequential path through the concurrent history that respects our
  sequential model, if we do we know that the concurrent execution is correct.

How it works
------------

Concurrent history
------------------

![](./images/concurrent_counter.svg){ width=400px }

Possible interleaving 1
-----------------------

![](./images/concurrent_counter_get_1_3.svg){ width=400px }

- `< incr 1, get, incr 2, get >`

Possible interleaving 2
-----------------------

![](./images/concurrent_counter_get_3_3.svg){ width=400px }

- `< incr 1, incr 2, get, get >`

Code
----

> {-# LANGUAGE DeriveFunctor #-}
> {-# LANGUAGE ScopedTypeVariables #-}
> {-# LANGUAGE DeriveFoldable #-}

> module Lec02ConcurrentSMTesting where

> import Control.Concurrent (ThreadId, threadDelay, myThreadId)
> import Control.Concurrent.Async (mapConcurrently)
> import Control.Concurrent.STM (TQueue, flushTQueue, atomically, newTQueueIO, writeTQueue)
> import Control.Monad (replicateM_, unless)
> import Data.IORef (atomicModifyIORef')
> import Data.List (permutations)
> import Data.Tree (Forest, Tree(Node), drawForest)
> import System.Random (randomRIO)
> import Test.QuickCheck (Property, Gen, classify, shrinkList, tabulate, counterexample,
>                         mapSize, sized, forAllShrinkShow, suchThat, vectorOf, chooseInt,
>                         sample, quickCheck)
> import Test.QuickCheck.Monadic (PropertyM, run, assert, monitor, monadicIO)
> import Test.HUnit (Assertion, assertBool)

We will reuse the counter SUT and model from the previous lecture.

> import Lec01SMTesting (Counter(Counter), Model, Command(Get, Incr), Response(Int, Unit),
>                        initModel, get,  step, newCounter, exec, shrinkCommand, genCommand)

In order to do concurrent testing we need to generate concurrent programs
though. In the sequential case a program was merely a list of commands, in the
concurrent case a program is a list of lists of commands, where the inner list
is what is supposed to be done concurrently.

> newtype ConcProgram = ConcProgram { unConcProgram :: [[Command]] }
>   deriving Show

> forAllConcProgram :: (ConcProgram -> Property) -> Property
> forAllConcProgram k =
>   forAllShrinkShow (genConcProgram m) (shrinkConcProgram m) prettyConcProgram k
>   where
>     m = initModel

When generating concurrent programs we proceed in concurrent "chunks", each
chunk is between 2 and 5 commands and each command will be executed concurrently
in a separate thread. In order for a command to be valid in a chunk it needs to
be safe to execute said command independent of the order in which the rest of
the commands are executed.

For a simple counter all commands are trivially safe, but imagine if we were
working with for example filesystem commands then it might be unsafe to remove
and rename a file in the same chunk.

When we are done generating a chunk, we advance the model by all commands in
that chunk before we continue generating the next one.

> genConcProgram :: Model -> Gen ConcProgram
> genConcProgram m0 = sized (go m0 [])
>   where
>     go :: Model -> [[Command]] -> Int -> Gen ConcProgram
>     go m acc sz | sz <= 0   = return (ConcProgram (reverse acc))
>                 | otherwise = do
>                     n <- chooseInt (2, 5)
>                     cmds <- vectorOf n genCommand `suchThat` concSafe m
>                     go (advanceModel m cmds) (cmds : acc) (sz - n)

> advanceModel :: Model -> [Command] -> Model
> advanceModel m cmds = foldl (\ih cmd -> fst (step ih cmd)) m cmds

> concSafe :: Model -> [Command] -> Bool
> concSafe m = all (validProgram m) . permutations

> validProgram :: Model -> [Command] -> Bool
> validProgram _model _cmds = True

> validConcProgram :: Model -> ConcProgram -> Bool
> validConcProgram m0 (ConcProgram cmdss0) = go m0 True cmdss0
>   where
>     go :: Model -> Bool -> [[Command]] -> Bool
>     go _m False _              = False
>     go _m acc   []             = acc
>     go m _acc   (cmds : cmdss) = go (advanceModel m cmds) (concSafe m cmds) cmdss

Shrinking concurrent programs is a bit more involved then below if we want to
get nice minimal counterexamples, we'll get back to this in one of the
exercises.

> shrinkConcProgram :: Model -> ConcProgram -> [ConcProgram]
> shrinkConcProgram m
>   = filter (validConcProgram m)
>   . map ConcProgram
>   . filter (not . null)
>   . shrinkList (shrinkList shrinkCommand)
>   . unConcProgram

> prettyConcProgram :: ConcProgram -> String
> prettyConcProgram = show

We cannot (easily) check for correctness while we are executing a concurrent
program, instead we merely collect a concurrent history while executing and
check this history after execution is done.

A history contains a chronologically ordered sequence of events of when a
command was invoked and on what thread or process id, and when the command
finished again together with which thread or process id. A single thread or
process id may only be invoking a single command at the time.

> newtype History' cmd resp = History [Operation' cmd resp]
>   deriving (Show, Functor, Foldable)

> type History = History' Command Response

> newtype Pid = Pid Int
>   deriving (Eq, Ord, Show)

> data Operation' cmd resp
>   = Invoke Pid cmd
>   | Ok     Pid resp
>   deriving (Show, Functor, Foldable)

> type Operation = Operation' Command Response

> toPid :: ThreadId -> Pid
> toPid tid = Pid (read (drop (length ("ThreadId " :: String)) (show tid)))

> appendHistory :: TQueue (Operation' cmd resp) -> Operation' cmd resp -> IO ()
> appendHistory hist op = atomically (writeTQueue hist op)

When executing, the threads involved in the execution have a shared/concurrent
queue which they append their invocation and completions to. From the concurrent
queue we get our concurrent history.

> concExec :: TQueue Operation -> Counter -> Command -> IO ()
> concExec queue counter cmd = do
>   pid <- toPid <$> myThreadId
>   appendHistory queue (Invoke pid cmd)
>   -- Adds some entropy to the possible interleavings.
>   sleep <- randomRIO (0, 5)
>   threadDelay sleep
>   resp <- exec counter cmd -- threadSafeExec counter cmd -- NOTE: Fix the race condition by uncommenting.
>   atomically (writeTQueue queue (Ok pid resp))

> threadSafeExec :: Counter -> Command -> IO Response
> threadSafeExec c cmd = case cmd of
>   Incr i -> Unit <$> threadSafeIncr c i
>   Get    -> Int  <$> get c
>   where
>     threadSafeIncr (Counter ref) i = atomicModifyIORef' ref (\j -> (i + j, ()))

With a concurrent history we can generate all possible single-threaded
executions.

> interleavings :: History' cmd resp -> Forest (cmd, resp)
> interleavings (History [])  = []
> interleavings (History ops0) =
>   [ Node (cmd, resp) (interleavings (History ops'))
>   | (tid, cmd)   <- takeInvocations ops0
>   , (resp, ops') <- findResponse tid
>                       (filter1 (not . matchInvocation tid) ops0)
>   ]
>   where
>     takeInvocations :: [Operation' cmd resp] -> [(Pid, cmd)]
>     takeInvocations []                         = []
>     takeInvocations ((Invoke pid cmd)   : ops) = (pid, cmd) : takeInvocations ops
>     takeInvocations ((Ok    _pid _resp) : _)   = []

>     findResponse :: Pid -> [Operation' cmd resp] -> [(resp, [Operation' cmd resp])]
>     findResponse _pid []                                   = []
>     findResponse  pid ((Ok pid' resp) : ops) | pid == pid' = [(resp, ops)]
>     findResponse  pid (op             : ops)               =
>       [ (resp, op : ops') | (resp, ops') <- findResponse pid ops ]

>     matchInvocation :: Pid -> Operation' cmd resp -> Bool
>     matchInvocation pid (Invoke pid' _cmd) = pid == pid'
>     matchInvocation _   _                  = False

>     filter1 :: (a -> Bool) -> [a] -> [a]
>     filter1 _ []                   = []
>     filter1 p (x : xs) | p x       = x : filter1 p xs
>                        | otherwise = xs

If any one of the single-threaded executions respects the state machine model,
then the concurrent execution is correct. This correctness criteria is the main
result from the "linearizability" paper linked to below.

> linearisable :: forall model cmd resp. Eq resp
>              => (model -> cmd -> (model, resp)) -> model -> Forest (cmd, resp) -> Bool
> linearisable step0 model0 = any' (go model0)
>   where
>     go :: model -> Tree (cmd, resp) -> Bool
>     go model (Node (cmd, resp) ts) =
>       let
>         (model', resp') = step0 model cmd
>       in
>         resp == resp' && any' (go model') ts

>     any' :: (a -> Bool) -> [a] -> Bool
>     any' _p [] = True
>     any'  p xs = any p xs

We now have all the pieces necessary to implement our concurrent property.

Note that in order to avoid being unlucky with the execution interleavings we
actually execute the same generated test case ten times.

> prop_concurrent :: Property
> prop_concurrent = mapSize (min 20) $
>   forAllConcProgram $ \(ConcProgram cmdss) -> monadicIO $ do
>     monitor (classifyCommandsLength (concat cmdss))
>     -- Rerun a couple of times, to avoid being lucky with the interleavings.
>     monitor (tabulate "Commands" (map constructorString (concat cmdss)))
>     monitor (tabulate "Number of concurrent commands" (map (show . length) cmdss))
>     replicateM_ 10 $ do
>       counter <- run newCounter
>       queue <- run newTQueueIO
>       run (mapM_ (mapConcurrently (concExec queue counter)) cmdss)
>       hist <- History <$> run (atomically (flushTQueue queue))
>       assertWithFail (linearisable step initModel (interleavings hist)) (prettyHistory hist)
>   where
>     constructorString :: Command -> String
>     constructorString Incr {} = "Incr"
>     constructorString Get  {} = "Get"

> assertWithFail :: Monad m => Bool -> String -> PropertyM m ()
> assertWithFail condition msg = do
>   unless condition $
>     monitor (counterexample ("Failed: " ++ msg))
>   assert condition

> classifyCommandsLength :: [cmd] -> Property -> Property
> classifyCommandsLength cmds
>   = classify (length cmds == 0)                        "length commands: 0"
>   . classify (0   < length cmds && length cmds <= 10)  "length commands: 1-10"
>   . classify (10  < length cmds && length cmds <= 50)  "length commands: 11-50"
>   . classify (50  < length cmds && length cmds <= 100) "length commands: 51-100"
>   . classify (100 < length cmds && length cmds <= 200) "length commands: 101-200"
>   . classify (200 < length cmds && length cmds <= 500) "length commands: 201-500"
>   . classify (500 < length cmds)                       "length commands: >501"

> prettyHistory :: (Show cmd, Show resp) => History' cmd resp -> String
> prettyHistory = show

> displayInterleavings :: (Show cmd, Show resp) => History' cmd resp -> IO ()
> displayInterleavings = putStrLn . drawForest . fmap (fmap show) . interleavings

Regression testing
------------------

> assertHistory :: String -> History -> Assertion
> assertHistory _msg hist =
>   assertBool (prettyHistory hist) (linearisable step initModel (interleavings hist))

Demo script
-----------

```
  > sample (genConcProgram initModel)
  ConcProgram []
  ConcProgram [[Incr 2,Incr 2,Get,Incr 0,Get]]
  ConcProgram [[Incr 3,Get,Incr 1,Incr 0]]
  ConcProgram [[Incr (-1),Get],[Get,Get],[Get,Get]]
  ConcProgram [[Get,Get,Incr 2,Get],[Get,Get,Incr (-5),Incr 8,Get]]
  ConcProgram [[Incr (-5),Incr (-1),Incr 7,Get],[Incr 3,Get,Get],[Get,Incr 5,Incr 0,Get,Incr 6]]
  ConcProgram [[Get,Get,Get,Get,Get],[Get,Get,Incr (-3),Incr 5],[Incr (-11),Incr (-6),Incr (-2)]]
  ConcProgram [[Incr 13,Get,Incr (-9)],[Incr (-13),Incr (-5),Incr (-4),Get],[Get,Incr 12,Incr 1,Incr 2,Get],[Incr (-10),Incr 10,Incr 11]]
  ConcProgram [[Get,Get,Get,Incr 4],[Get,Incr (-16),Get,Incr (-5)],[Incr (-9),Get,Incr (-10),Get,Incr 13],[Incr 11,Get,Get,Get]]
  ...

  > quickCheck prop_concurrent
  ConcProgram [[Incr 0,Incr 14],[Get,Get,Get]]

  Failed: History [Invoke (Pid 296705) (Incr 0),Invoke (Pid 296707) (Incr 14),Ok (Pid 296707) (Unit ()),Ok (Pid 296705) (Unit ()),Invoke (Pid 296709) Get,Invoke (Pid 296711) Get,Ok (Pid 296709) (Int 0),Invoke (Pid 296713) Get,Ok (Pid 296711) (Int 0),Ok (Pid 296713) (Int 0)]

  Pid 296705: |---- Incr 0 -------|
  Pid 296707:   |--- Incr 14 ---|
  Pid 296709:                       |--- Get => 0 ---|
  Pid 296711:                         |---- Get => 0----------|
  Pid 296713:                                          |---- Get => 0 ---|

  > displayInterleavings (History [Invoke (Pid 296705) (Incr 0),Invoke (Pid 296707) (Incr 14),Ok (Pid 296707) (Unit ()),Ok (Pid 296705) (Unit ()),Invoke (Pid 296709) Get,Invoke (Pid 296711) Get,Ok (Pid 296709) (Int 0),Invoke (Pid 296713) Get,Ok (Pid 296711) (Int 0),Ok (Pid 296713) (Int 0)])

(Incr 0,Unit ())
|
`- (Incr 14,Unit ())
   |
   +- (Get,Int 0)
   |  |
   |  +- (Get,Int 0)
   |  |  |
   |  |  `- (Get,Int 0)
   |  |
   |  `- (Get,Int 0)
   |     |
   |     `- (Get,Int 0)
   |
   `- (Get,Int 0)
      |
      `- (Get,Int 0)
         |
         `- (Get,Int 0)

(Incr 14,Unit ())
|
`- (Incr 0,Unit ())
   |
   +- (Get,Int 0)
   |  |
   |  +- (Get,Int 0)
   |  |  |
   |  |  `- (Get,Int 0)
   |  |
   |  `- (Get,Int 0)
   |     |
   |     `- (Get,Int 0)
   |
   `- (Get,Int 0)
      |
      `- (Get,Int 0)
         |
         `- (Get,Int 0)
```

Discussion
----------

- Black- vs white-box testing: if you think of the SUT as a box, then checking
  for race conditions using linearisability requires no insight or changes to
  what is going on in the box, i.e. it's a black-box technique. On the other
  hand, if one is ready to give access to or make changes to the box to
  facilitate testing then we may apply so called white-box techniques. An
  example of a white-box technique is Go's [race
  detector](https://go.dev/blog/race-detector) or the Haskell library
  [dejafu](https://hackage.haskell.org/package/dejafu).

- Linearisability is by no means an intuitive concept, it will take a while
  before it sinks in. Meanwhile, feel free to ask questions.

Exercises
---------

0. Can you figure out ways to improve the shrinking? (Hint: see parallel
   shrinking in
   [`quickcheck-state-machine`](https://hackage.haskell.org/package/quickcheck-state-machine).)

1. How can you test that the shrinking is good/optimal? (Hint: see how
   `labelledExamples` is used in the [*An in-depth look at
   quickcheck-state-machine*](https://www.well-typed.com/blog/2019/01/qsm-in-depth/)
   blog post by Edsko de Vries and [*Building on developers' intuitions to
   create effective property-based
   tests*](https://www.youtube.com/watch?v=NcJOiQlzlXQ) talk by John Hughes)

2. Display counterexample in a way that makes it easier to see what went wrong.
   (Hint: perhaps taking inspiration from the diagrams in the beginning of this
   lecture.)

See also
--------

- [*Finding Race Conditions in Erlang with QuickCheck and
  PULSE*](http://www.cse.chalmers.se/~nicsma/papers/finding-race-conditions.pdf)
  (2009) ([video](https://vimeo.com/6638041)) -- this paper describes how
  Erlang's (closed source) version QuickCheck does concurrent testing (it was
  the first property-based testing library to do so);

- [*Linearizability: a correctness condition for concurrent
  objects*](https://cs.brown.edu/~mph/HerlihyW90/p463-herlihy.pdf)] (1990), this
  is a classic paper that describes the main technique of the concurrent
  property;

- Kyle "aphyr" Kingsbury's blogposts about Jepsen, which also uses
  linearisability, and has found [bugs](http://jepsen.io/analyses) in many
  distributed systems:

    + [Knossos: Redis and
      linearizability](https://aphyr.com/posts/309-knossos-redis-and-linearizability);

    + [Strong consistency
      models](https://aphyr.com/posts/313-strong-consistency-models);

    + [Computational techniques in
      Knossos](https://aphyr.com/posts/314-computational-techniques-in-knossos);

    + [Serializability, linearizability, and
      locality](https://aphyr.com/posts/333-serializability-linearizability-and-locality).

Summary
-------

When you got a state machine model of a program, you can get race conditions
testing for free.
