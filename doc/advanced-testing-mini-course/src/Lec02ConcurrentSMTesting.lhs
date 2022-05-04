> {-# LANGUAGE DeriveFunctor #-}
> {-# LANGUAGE ScopedTypeVariables #-}
> {-# LANGUAGE DeriveFoldable #-}

> module Lec02ConcurrentSMTesting where

> import Control.Concurrent
> import Control.Concurrent.Async
> import Control.Concurrent.STM
> import Control.Concurrent.STM.TQueue
> import Control.Monad
> import Data.List (permutations)
> import Data.Tree (Forest, Tree(Node))
> import System.Random
> import Test.QuickCheck
> import Test.QuickCheck.Monadic
> import Test.HUnit hiding (assert)

> import Lec01SMTesting

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

> newtype ConcProgram = ConcProgram { unConcProgram :: [[Command]] }
>   deriving Show

> forAllConcProgram :: (ConcProgram -> Property) -> Property
> forAllConcProgram k =
>   forAllShrinkShow (genConcProgram m) (shrinkConcProgram m) prettyConcProgram k
>   where
>     m = initModel

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
> concSafe m0 = all (validProgram m0) . permutations

> validConcProgram :: Model -> ConcProgram -> Bool
> validConcProgram m0 (ConcProgram cmdss0) = go m0 True cmdss0
>   where
>     go :: Model -> Bool -> [[Command]] -> Bool
>     go m False _              = False
>     go m acc   []             = acc
>     go m acc   (cmds : cmdss) = go (advanceModel m cmds) (concSafe m cmds) cmdss

> shrinkConcProgram :: Model -> ConcProgram -> [ConcProgram]
> shrinkConcProgram m
>   = filter (validConcProgram m)
>   . map ConcProgram
>   . filter (not . null)
>   . shrinkList (shrinkList shrinkCommand)
>   . unConcProgram

> prettyConcProgram :: ConcProgram -> String
> prettyConcProgram = show

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

> concExec :: TQueue Operation -> Counter -> Command -> IO ()
> concExec queue counter cmd = do
>   pid <- toPid <$> myThreadId
>   atomically (writeTQueue queue (Invoke pid cmd))
>   -- Adds some entropy to the possible interleavings.
>   sleep <- randomRIO (0, 5)
>   threadDelay sleep
>   resp <- exec counter cmd
>   atomically (writeTQueue queue (Ok pid resp))

Generate all possible single-threaded executions from the concurrent history.

> interleavings :: History' cmd resp -> Forest (cmd, resp)
> interleavings (History [])  = []
> interleavings (History ops) =
>   [ Node (cmd, resp) (interleavings (History ops'))
>   | (tid, cmd)   <- takeInvocations ops
>   , (resp, ops') <- findResponse tid
>                       (filter1 (not . matchInvocation tid) ops)
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
then the concurrent execution is correct.

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
>     classifyCommandsLength :: [Command] -> Property -> Property
>     classifyCommandsLength cmds
>       = classify (length cmds == 0)                        "length commands: 0"
>       . classify (0   < length cmds && length cmds <= 10)  "length commands: 1-10"
>       . classify (10  < length cmds && length cmds <= 50)  "length commands: 11-50"
>       . classify (50  < length cmds && length cmds <= 100) "length commands: 51-100"
>       . classify (100 < length cmds && length cmds <= 200) "length commands: 101-200"
>       . classify (200 < length cmds && length cmds <= 500) "length commands: 201-500"
>       . classify (500 < length cmds)                       "length commands: >501"

>     constructorString :: Command -> String
>     constructorString Incr {} = "Incr"
>     constructorString Get  {} = "Get"

>     assertWithFail :: Monad m => Bool -> String -> PropertyM m ()
>     assertWithFail condition msg = do
>       unless condition $
>         monitor (counterexample ("Failed: " ++ msg))
>       assert condition

> prettyHistory :: (Show cmd, Show resp) => History' cmd resp -> String
> prettyHistory = show

Regression testing
------------------

> assertHistory :: String -> History -> Assertion
> assertHistory msg hist =
>   assertBool (prettyHistory hist) (linearisable step initModel (interleavings hist))


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


See also
--------

- [*Finding Race Conditions in Erlang with QuickCheck and
  PULSE*](http://www.cse.chalmers.se/~nicsma/papers/finding-race-conditions.pdf)
  ([video](https://vimeo.com/6638041)) -- this is the first paper to describe
  how Erlang's (closed source) QuickCheck works (including the parallel
  testing);

- [*Linearizability: a correctness condition for concurrent
  objects*](https://cs.brown.edu/~mph/HerlihyW90/p463-herlihy.pdf)], this is a
  classic paper that describes the main technique of the parallel property;

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
