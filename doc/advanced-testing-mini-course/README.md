``` {.haskell .literate}
module ATMC.Lec01SMTesting where
```

``` {.haskell .literate}
import Control.Monad.IO.Class
import Data.IORef
import Test.QuickCheck
import Test.QuickCheck.Monadic
import Test.HUnit
```

# State machine testing

## Motivation

-   Testing: "the process of using or trying something to see if it works, is suitable, obeys the rules, etc." -- Cambridge dictionary

-   In order to check that the software under test (SUT) obeys the rules we must first write down the rules

-   State machine specifications are one of many ways to formally "write down the rules"

## Plan

XXX: ...

-   Use a state machine (pure function from input and state to output and an updated state) to model stateful (impure) systems

## SUT

``` {.haskell .literate}
newtype Counter = Counter (IORef Int)
```

``` {.haskell .literate}
newCounter :: IO Counter
newCounter = do
  ref <- newIORef 0
  return (Counter ref)
```

``` {.haskell .literate}
incr :: Counter -> IO ()
incr (Counter ref) = do
  n <- readIORef ref
  writeIORef ref (n + 1)
```

``` {.haskell .literate}
get :: Counter -> IO Int
get (Counter ref) = readIORef ref
```

## State machine model/specification/fake

``` {.haskell .literate}
newtype FakeCounter = FakeCounter Int
```

``` {.haskell .literate}
fakeIncr :: FakeCounter -> (FakeCounter, ())
fakeIncr (FakeCounter i) = (FakeCounter (i + 1), ())
```

``` {.haskell .literate}
fakeGet :: FakeCounter -> (FakeCounter, Int)
fakeGet (FakeCounter i) = (FakeCounter i, i)
```

``` {.haskell .literate}
data Command = Incr | Get
  deriving (Eq, Show)
```

``` {.haskell .literate}
data Response = Unit () | Int Int
  deriving (Eq, Show)
```

``` {.haskell .literate}
type Model = FakeCounter
```

``` {.haskell .literate}
initModel :: Model
initModel = FakeCounter 0
```

``` {.haskell .literate}
step :: Model -> Command -> (Model, Response)
step m cmd = case cmd of
  Incr -> Unit <$> fakeIncr m
  Get  -> Int  <$> fakeGet m
```

``` {.haskell .literate}
exec :: Counter -> Command -> IO Response
exec c cmd = case cmd of
  Incr -> Unit <$> incr c
  Get  -> Int  <$> get c
```

``` {.haskell .literate}
newtype Program = Program [Command]
  deriving Show
```

``` {.haskell .literate}
genCommand :: Gen Command
genCommand = elements [Incr, Get]
```

``` {.haskell .literate}
genProgram :: Model -> Gen Program
genProgram _m = Program <$> listOf genCommand
```

``` {.haskell .literate}
samplePrograms :: IO [Program]
samplePrograms = sample' (genProgram initModel)
```

``` {.haskell .literate}
validProgram :: Model -> [Command] -> Bool
validProgram _mode _cmds = True
```

``` {.haskell .literate}
shrinkCommand :: Command -> [Command]
shrinkCommand _cmd = []
```

``` {.haskell .literate}
shrinkProgram :: Program -> [Program]
shrinkProgram _prog = [] -- Exercises.
```

``` {.haskell .literate}
forallPrograms :: (Program -> Property) -> Property
forallPrograms p =
  forAllShrink (genProgram initModel) shrinkProgram p
```

``` {.haskell .literate}
prop_counter :: Property
prop_counter = forallPrograms $ \prog -> monadicIO $ do
  c <- run newCounter
  let m = initModel
  runProgram c m prog
```

``` {.haskell .literate}
runProgram :: MonadIO m => Counter -> Model -> Program -> m Bool
runProgram c0 m0 (Program cmds) = go c0 m0 cmds
  where
     go _c _m []           = return True
     go  c  m (cmd : cmds) = do
       resp <- liftIO (exec c cmd)
       let (m', resp') = step m cmd
       if resp == resp'
       then go c m' cmds
       else return False
```

## Regression tests

``` {.haskell .literate}
assertProgram :: String -> Program -> Assertion
assertProgram msg prog = do
  c <- newCounter
  let m = initModel
  b <- runProgram c m prog
  assertBool msg b
```

## Discussion

-   The specification is longer than the SUT!? For something as simple as a counter, this is true, but for any "real world" system that e.g. persists to disk the model will likely be smaller by an order of magnitude or more.

-   Why state machines over other forms of specifications? E.g. unit test-suite.

    -   First of all, a bunch of unit tests are not a specification in the same way that a bunch of examples in math are not a proposition/theorem.

    -   Stateless (or pure) property-based testing tries to *approximate* proof by induction in math. For example the following is the proposition that addition is associative for integers, *forall i j k. (i + j) + k == i + (j + k)*. It looks almost exactly like the property you'd write in a property-based test, but of course this test passing isn't a proof of the proposition, still a step in the right direction if we want to be serious about program correctness.

    -   XXX: Stateful property-based testing using state machines, like we seen in this lecture, tries to approximate proof by structural induction on the sequence of inputs. Or inductive invarint method?!

    -   Executable (as the REPL exercise shows, but also more on this later)

    -   Same state machine specification can be used for concurrent testing (Lec 2)

    -   Mental model

    -   Already heavily used in distributed systems (later we'll see how the model becomes the implementation)

## Excerises

0.  If you're not comfortable with Haskell, port the above code to your favorite programming language.

1.  Add a `Reset` `Command` which resets the counter to its initial value.

2.  Implement shrinking for programs.

3.  Write a REPL for the state machine. Start with the initial state, prompt the user for a command, apply the provided command to the step function and display the response as well as the new state, rinse and repeat.

    (For a SUT as simple as a counter this doesn't make much sense, but when the SUT get more complicated it might make sense to develope the state machine specification first, demo it using something like a REPL or some other simple UI before even starting to implement the real thing.)

4.  Collect timing information about how long each command takes to execute on average.

## See also

-   The original QuickCheck [paper](https://dl.acm.org/doi/pdf/10.1145/357766.351266) by Koen Claessen and John Hughes (2000) that introduced property-based testing in Haskell.

-   John Hughes' Midlands Graduate School 2019 [course](http://www.cse.chalmers.se/~rjmh/MGS2019/) on property-based testing, which covers the basics of state machine modelling and testing. It also contains a minimal implementation of a state machine testing library built on top of Haskell's QuickCheck;

-   Lamport's [Computation and State Machines](https://www.microsoft.com/en-us/research/publication/computation-state-machines/)

    (2008) 

-   "Can one generalize Turing machines so that any algorithm, never mind how ab- stract, can be modeled by a generalized machine very closely and faithfully?"

    Perhaps somewhat surprisingly it turns out that the answer is yes, and the generalisation is a state machine! (This means that in some sense the state machine is the ultimate model?!)

    For details see Gurevich's [generalisation](http://delta-apache-vm.cs.tau.ac.il/~nachumd/models/gurevich.pdf) of the Church-Turing thesis.

---

``` {.haskell .literate}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveFoldable #-}
```

``` {.haskell .literate}
module ATMC.Lec02ConcurrentSMTesting where
```

``` {.haskell .literate}
import Control.Concurrent
import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Concurrent.STM.TQueue
import Control.Monad
import Data.List (permutations)
import Data.Tree (Forest, Tree(Node))
import System.Random
import Test.QuickCheck
import Test.QuickCheck.Monadic
import Test.HUnit hiding (assert)
```

``` {.haskell .literate}
import ATMC.Lec01SMTesting
```

# Concurrent state machine testing with linearisability

## Motivation

-   In the previous chapter we saw how to test if a sequential (single-threaded) program respects some state machine specification

-   Next we show how the *same* specification can be used to check if a concurrent execution is correct using linearisability

-   E.g. counters are often shared among different threads, how can we test that the counter implementation is thread-safe?

``` {.haskell .literate}
newtype ConcProgram = ConcProgram { unConcProgram :: [[Command]] }
  deriving Show
```

``` {.haskell .literate}
forAllConcProgram :: (ConcProgram -> Property) -> Property
forAllConcProgram k =
  forAllShrinkShow (genConcProgram m) (shrinkConcProgram m) prettyConcProgram k
  where
    m = initModel
```

``` {.haskell .literate}
genConcProgram :: Model -> Gen ConcProgram
genConcProgram m0 = sized (go m0 [])
  where
    go :: Model -> [[Command]] -> Int -> Gen ConcProgram
    go m acc sz | sz <= 0   = return (ConcProgram (reverse acc))
                | otherwise = do
                    n <- chooseInt (2, 5)
                    cmds <- vectorOf n genCommand `suchThat` concSafe m
                    go (advanceModel m cmds) (cmds : acc) (sz - n)
```

``` {.haskell .literate}
advanceModel :: Model -> [Command] -> Model
advanceModel m cmds = foldl (\ih cmd -> fst (step ih cmd)) m cmds
```

``` {.haskell .literate}
concSafe :: Model -> [Command] -> Bool
concSafe m0 = all (validProgram m0) . permutations
```

``` {.haskell .literate}
validConcProgram :: Model -> ConcProgram -> Bool
validConcProgram m0 (ConcProgram cmdss0) = go m0 True cmdss0
  where
    go :: Model -> Bool -> [[Command]] -> Bool
    go m False _              = False
    go m acc   []             = acc
    go m acc   (cmds : cmdss) = go (advanceModel m cmds) (concSafe m cmds) cmdss
```

``` {.haskell .literate}
shrinkConcProgram :: Model -> ConcProgram -> [ConcProgram]
shrinkConcProgram m
  = filter (validConcProgram m)
  . map ConcProgram
  . filter (not . null)
  . shrinkList (shrinkList shrinkCommand)
  . unConcProgram
```

``` {.haskell .literate}
prettyConcProgram :: ConcProgram -> String
prettyConcProgram = show
```

``` {.haskell .literate}
newtype History' cmd resp = History [Operation' cmd resp]
  deriving (Show, Functor, Foldable)
```

``` {.haskell .literate}
type History = History' Command Response
```

``` {.haskell .literate}
newtype Pid = Pid Int
  deriving (Eq, Ord, Show)
```

``` {.haskell .literate}
data Operation' cmd resp
  = Invoke Pid cmd
  | Ok     Pid resp
  deriving (Show, Functor, Foldable)
```

``` {.haskell .literate}
type Operation = Operation' Command Response
```

``` {.haskell .literate}
toPid :: ThreadId -> Pid
toPid tid = Pid (read (drop (length ("ThreadId " :: String)) (show tid)))
```

``` {.haskell .literate}
concExec :: TQueue Operation -> Counter -> Command -> IO ()
concExec queue counter cmd = do
  pid <- toPid <$> myThreadId
  atomically (writeTQueue queue (Invoke pid cmd))
  -- Adds some entropy to the possible interleavings.
  sleep <- randomRIO (0, 5)
  threadDelay sleep
  resp <- exec counter cmd
  atomically (writeTQueue queue (Ok pid resp))
```

Generate all possible single-threaded executions from the concurrent history.

``` {.haskell .literate}
interleavings :: History' cmd resp -> Forest (cmd, resp)
interleavings (History [])  = []
interleavings (History ops) =
  [ Node (cmd, resp) (interleavings (History ops'))
  | (tid, cmd)   <- takeInvocations ops
  , (resp, ops') <- findResponse tid
                      (filter1 (not . matchInvocation tid) ops)
  ]
  where
    takeInvocations :: [Operation' cmd resp] -> [(Pid, cmd)]
    takeInvocations []                         = []
    takeInvocations ((Invoke pid cmd)   : ops) = (pid, cmd) : takeInvocations ops
    takeInvocations ((Ok    _pid _resp) : _)   = []
```

``` {.haskell .literate}
    findResponse :: Pid -> [Operation' cmd resp] -> [(resp, [Operation' cmd resp])]
    findResponse _pid []                                   = []
    findResponse  pid ((Ok pid' resp) : ops) | pid == pid' = [(resp, ops)]
    findResponse  pid (op             : ops)               =
      [ (resp, op : ops') | (resp, ops') <- findResponse pid ops ]
```

``` {.haskell .literate}
    matchInvocation :: Pid -> Operation' cmd resp -> Bool
    matchInvocation pid (Invoke pid' _cmd) = pid == pid'
    matchInvocation _   _                  = False
```

``` {.haskell .literate}
    filter1 :: (a -> Bool) -> [a] -> [a]
    filter1 _ []                   = []
    filter1 p (x : xs) | p x       = x : filter1 p xs
                       | otherwise = xs
```

If any one of the single-threaded executions respects the state machine model, then the concurrent execution is correct.

``` {.haskell .literate}
linearisable :: forall model cmd resp. Eq resp
             => (model -> cmd -> (model, resp)) -> model -> Forest (cmd, resp) -> Bool
linearisable step0 model0 = any' (go model0)
  where
    go :: model -> Tree (cmd, resp) -> Bool
    go model (Node (cmd, resp) ts) =
      let
        (model', resp') = step0 model cmd
      in
        resp == resp' && any' (go model') ts
```

``` {.haskell .literate}
    any' :: (a -> Bool) -> [a] -> Bool
    any' _p [] = True
    any'  p xs = any p xs
```

``` {.haskell .literate}
prop_concurrent :: Property
prop_concurrent = mapSize (min 20) $
  forAllConcProgram $ \(ConcProgram cmdss) -> monadicIO $ do
    monitor (classifyCommandsLength (concat cmdss))
    -- Rerun a couple of times, to avoid being lucky with the interleavings.
    monitor (tabulate "Commands" (map constructorString (concat cmdss)))
    monitor (tabulate "Number of concurrent commands" (map (show . length) cmdss))
    replicateM_ 10 $ do
      counter <- run newCounter
      queue <- run newTQueueIO
      run (mapM_ (mapConcurrently (concExec queue counter)) cmdss)
      hist <- History <$> run (atomically (flushTQueue queue))
      assertWithFail (linearisable step initModel (interleavings hist)) (prettyHistory hist)
  where
    classifyCommandsLength :: [Command] -> Property -> Property
    classifyCommandsLength cmds
      = classify (length cmds == 0)                        "length commands: 0"
      . classify (0   < length cmds && length cmds <= 10)  "length commands: 1-10"
      . classify (10  < length cmds && length cmds <= 50)  "length commands: 11-50"
      . classify (50  < length cmds && length cmds <= 100) "length commands: 51-100"
      . classify (100 < length cmds && length cmds <= 200) "length commands: 101-200"
      . classify (200 < length cmds && length cmds <= 500) "length commands: 201-500"
      . classify (500 < length cmds)                       "length commands: >501"
```

``` {.haskell .literate}
    constructorString :: Command -> String
    constructorString Incr {} = "Incr"
    constructorString Get  {} = "Get"
```

``` {.haskell .literate}
    assertWithFail :: Monad m => Bool -> String -> PropertyM m ()
    assertWithFail condition msg = do
      unless condition $
        monitor (counterexample ("Failed: " ++ msg))
      assert condition
```

``` {.haskell .literate}
prettyHistory :: History -> String
prettyHistory = show
```

## Regression testing

``` {.haskell .literate}
assertHistory :: String -> History -> Assertion
assertHistory msg hist =
  assertBool (prettyHistory hist) (linearisable step initModel (interleavings hist))
```

## Exercises

0.  Can you figure out ways to improve the shrinking? (Hint: see parallel shrinking in [`quickcheck-state-machine`](https://hackage.haskell.org/package/quickcheck-state-machine).)

1.  How can you test that the shrinking is good/optimal? (Hint: see how `labelledExamples` is used in the [*An in-depth look at quickcheck-state-machine*](https://www.well-typed.com/blog/2019/01/qsm-in-depth/) blog post by Edsko de Vries and [*Building on developers' intuitions to create effective property-based tests*](https://www.youtube.com/watch?v=NcJOiQlzlXQ) talk by John Hughes)

## See also

-   [*Finding Race Conditions in Erlang with QuickCheck and PULSE*](http://www.cse.chalmers.se/~nicsma/papers/finding-race-conditions.pdf) ([video](https://vimeo.com/6638041)) -- this is the first paper to describe how Erlang's (closed source) QuickCheck works (including the parallel testing);

-   [*Linearizability: a correctness condition for concurrent objects*](https://cs.brown.edu/~mph/HerlihyW90/p463-herlihy.pdf)\], this is a classic paper that describes the main technique of the parallel property;

-   Kyle "aphyr" Kingsbury's blogposts about Jepsen, which also uses linearisability, and has found [bugs](http://jepsen.io/analyses) in many distributed systems:

    -   [Knossos: Redis and linearizability](https://aphyr.com/posts/309-knossos-redis-and-linearizability);

    -   [Strong consistency models](https://aphyr.com/posts/313-strong-consistency-models);

    -   [Computational techniques in Knossos](https://aphyr.com/posts/314-computational-techniques-in-knossos);

    -   [Serializability, linearizability, and locality](https://aphyr.com/posts/333-serializability-linearizability-and-locality).

---

``` {.haskell .literate}
module ATMC.Lec03SMContractTesting where
```

``` {.haskell .literate}
import Data.IORef
```

# Consumer-driven contract testing using state machines

## Motivation

-   Components rarely exist in isolation, they almost always depend on some other component;

-   When we test we often want to test as if the component existed in isolation though, e.g. if component A depends on component B, we'd like to test B first and then *assume* that B is working when testing A;

-   Assumptions like these can be justified using so called *contract tests*.

## Plan

-   Following the pattern from lecture 1: make a SM based fake for B, use the fake as model to SM test the real implementation of B;

-   Use the fake of B in place of the real implementation of B inside the real implementation of A;

-   Make a SM model for A which contains the model of B and test the real implementaiton of A.

## Picture

                   Interface
                       |
        Consumer       |     Producer
                       |
           ----------> x-------->
                       |
      "Collaboration   |  Contract tests
          tests"       |

## SUT B: a queue (producer of the interface)

``` {.haskell .literate}
import ATMC.Lec03.QueueInterface
import ATMC.Lec03.Queue
import ATMC.Lec03.QueueTest
```

## SUT A: web service (consumer of the interface)

``` {.haskell .literate}
import ATMC.Lec03.Service
```

------------------------------------------------------------------------

## Consumer-driven contract tests

The job of contract tests are to ensure the accuracy of the mocks/test doubles you use of other components in your fast and deterministic integration tests.

Consumer-driven contract tests just means that the consumer of the mocked API writes the contract test inside the testsuite of the producer.

If component A and B are developed in different repos or by different teams, then the consumer of the API (in our case A consumes B's API) should write the contract test (hence *consumer-driven*).

That way:

1.  the fake of the consumed API is more to encode the assumptions that the consumer makes;

2.  if the implementation of the consumed API changes in a way that break the contract test that ensures that the fake is faithfully with regards to the real implementation, then the developers of the consumed API will get a failing test and thus a warning about the fact that some assumptions of the comsumer might have been broken.

## Discussion

Why not just spin up the real component B when testing component A?

-   Imagine B is a queue and the real implementation uses Kafka, then we'd need to start several processes...

-   Sometimes component B is slow to use (uses disk or network I/O)...

-   Sometimes component B is a third-party component which we can't redeploy or reset between test runs...

-   Often we want to be resilient at the level of component A in case component B fails, injecting faults in B to test this is much easier on a fake of B rather than on the real implementation of B (more on this in the next lecture).

-   Basically this is how the road towards slow and flaky tests starts. Don't go down that path! If you are thinking: "but some code is only exercised when the real component is deployed, e.g. configuration", then use [smoke tests](https://en.wikipedia.org/wiki/Smoke_testing_%28software%29) rather than integration tests with real components.

    Origin of the terminology: "The phrase smoke test comes from electronic hardware testing. You plug in a new board and turn on the power. If you see smoke coming from the board, turn off the power. You don't have to do any more testing."

    The software analogue: spin up component(s), wait for their status to become "ready", make some basic requests and see if they succeed.

    Acceptable if these are a bit flaky:

    -   Component spin up happens relatively rarely in production
    -   These tests will likely involve docker containers and networking, i.e. third-party infrastructure that sometimes is flaky

    "After code reviews, smoke testing is the most cost effective method for identifying and fixing defects in software." -- [Microsoft](https://docs.microsoft.com/en-us/previous-versions/ms182613(v=vs.80))

    For most software systems, between good contract tests and smoke tests there shouldn't be much of a gap for bugs to sneak in. For special cases, such as distributed systems, we will cover more comprehensive techniques in lecture 5.

## Exercises

0.  The fake/model of the queue is thread-safe, but the real implementation isn't! Fix that and do concurrent contract testing.

1.  Introduce an interface for all database interaction, move the current database implementation to `realDb` and introduce fake database instance of the interface.

2.  Write contract tests that ensure that the fake database faithfully represents the real one.

3.  Once the contract tests pass, switch out the real database for the fake one in the collabortation tests (the testsuite of the web service). Enable timing output in `ghci` with `:set +s`, crank up the number of tests that `QuickCheck` generates, and see if you notice any speed up in the test execution time.

## See also

-   For the difference between a fake and e.g. a mock see the following [article](https://www.martinfowler.com/bliki/TestDouble.html) by Martin Fowler;

-   For more on contract testing see this [article](https://martinfowler.com/bliki/ContractTest.html) and for more on their consumer-driven variant see the following [artcile](https://martinfowler.com/articles/consumerDrivenContracts.html);

-   [*Integrated Tests Are A Scam*](https://www.youtube.com/watch?v=fhFa4tkFUFw) talk by J.B. Rainsberger (2022), this a less ranty version of a talk with the same title that he [gave](https://www.youtube.com/watch?v=VDfX44fZoMc) at DevConFu in 2013.

## Summary

-   Using fakes enables to fast and determinstic integration tests and, as we shall see next, makes it easier to introduce faults when testing;

-   Contract tests justify the use of fakes, inplace of the real dependencies, when testing a SUT.

---

``` {.haskell .literate}
module ATMC.Lec04FaultInjection where
```

# Fault-injection

## Motivation

-   "almost all (92%) of the catastrophic system failures are the result of incorrect handling of non-fatal errors explicitly signaled in software. \[...\] in 58% of the catastrophic failures, the underlying faults could easily have been detected through simple testing of error handling code." -- [Simple Testing Can Prevent Most Critical Failures: An Analysis of Production Failures in Distributed Data-intensive Systems](http://www.eecg.toronto.edu/~yuan/papers/failure_analysis_osdi14.pdf)
    (2014) Yuan et al;

## Plan

-   Possible faults to inject for the queue
    -   write fails, e.g. queue is full
    -   read fails, e.g. bug in queue causes exception to be thrown
    -   read returns a malformed write which no longer deserialises, or has a valid client request id to send the response to

## Discussion

-   Can we not just inject real faults like Jepsen does? [`iptables`](https://linux.die.net/man/8/iptables) for dropping messages and network partitions, [`tc`](https://man7.org/linux/man-pages/man8/tc.8.html) for creating latency or simulating a slow connection on the network, [`(p)kill`](https://linux.die.net/man/1/kill) for killing processes, `kill -STOP   $pid` and `kill -CONT $pid` for pausing and resuming processes to simulate long I/O or GC pauses, [`libfaketime`](https://github.com/wolfcw/libfaketime) for clock-skews, etc?

    We could, after all Jepsen is a very successful at finding bugs in distributed databases using these techniques. However keep in mind exactly how Jepsen is used: typically companies hire Kyle Kingsbury for a couple of weeks/months, he writes the tests and runs them, analyses the results and writes a report.

    XXX:

    -   requires root, needs to be done in containers or vm which slows down and complicates start up
    -   non-deterministic
    -   slow
    -   ci flakiness
    -   blackbox

-   Can we contract test the fault injection? I.e. how do we know that the faults we inject correspond to real faults that can happen? How can we be sure to have covered all possible real faults?

    XXX:

    -   fault models, e.g. see: https://github.com/coilhq/tigerbeetle/blob/main/docs/DESIGN.md#fault-models

## Exercises

0.  Try to imagine how much more difficult it would be to write these tests without injecting the faults in the fake, but rather the real dependency.

## See also

-   \[*Why Is Random Testing Effective for Partition Tolerance Bugs?*(https://dl.acm.org/doi/pdf/10.1145/3158134) by Majumdar and Niksic
    (2018) 

---

``` {.haskell .literate}
module ATMC.Lec05SimulationTesting where
```

``` {.haskell .literate}
import ATMC.Lec05.EventLoop
```

# Simulation testing

## Motivation

-   "haven't tested foundation\[db\] in part because their testing appears to be waaaay more rigorous than mine." -- Kyle ["aphyr"](https://twitter.com/aphyr/status/405017101804396546) Kingsbury

-   ["Jepsen-proof engineering"](https://sled.rs/simulation.html)

## Idea

-   What interface does the event loop provide?
    -   Send/recv messages
    -   Timers
    -   File I/O
-   Implement this interface twice:
    1.  Real implementation
    2.  Simulation, where outgoing messages are intercepted and scheduled at random but deterministically using a seed (or dropped to simulate e.g. network partitions)

![simulation event loop](images/simulation-eventloop.svg)

The following pseudo code implementation of the event loop works for both the "real" deployment and simulation:

    while true {
      msg := deliver()
      out := recv(msg)
      send(out)
    }

by switching out the implementation of `send` from sending messages over the network to merely adding the message to priority queue, and switching out the implementation of `deliever` from listning on a socket to merely popping messages off the priority queue, then we can switch between a "real" deployment and simulation by merely switching between different implementations of the event loops interface (`send` and `deliver`).

## Exercises

0.  Add a way to record all inputs during production deployment

1.  Add a way to produce a history from the recorded inputs

2.  Add a debugger that works on the history, similar to the REPL from the first lecture

3.  Write a checker that works on histories that ensures that the safety properites from section 8 on correctness from [*Viewstamped Replication Revisited*](https://pmg.csail.mit.edu/papers/vr-revisited.pdf) by Barbara Liskov and James Cowling (2012)

-   https://making.pusher.com/fuzz-testing-distributed-systems-with-quickcheck/
-   https://fractalscapeblog.wordpress.com/2017/05/05/quickcheck-for-paxos/

## See also

-   Where do these ideas come from?

    The first reference we been able to find is the following concluding remark by Alan Perlis (the first recipient of the Turing Award) about a discussion about a paper on simulation testing by Brian Randell at the first [conference](http://homepages.cs.ncl.ac.uk/brian.randell/NATO/nato1968.PDF) on Software Engineering (this is where we got the term from) in 1968:

    "I'd like to read three sentences to close this issue.

         1. A software system can best be designed if the testing is interlaced with
            the designing instead of being used after the design.

         2. A simulation which matches the requirements contains the control which
            organizes the design of the system.

         3. Through successive repetitions of this process of interlaced testing and
            design the model ultimately becomes the software system itself. I think that it
            is the key of the approach that has been suggested, that there is no such
            question as testing things after the fact with simulation models, but that in
            effect the testing and the replacement of simulations with modules that are
            deeper and more detailed goes on with the simulation model controlling, as it
            were, the place and order in which these things are done."

-   The idea in its current shape and as applied to distributed systems was introduced(?), or at the very least popularised, by Will Wilson's talk [*Testing Distributed Systems w/ Deterministic Simulation*](https://www.youtube.com/watch?v=4fFDFbi3toc) at Strange Loop

    (2014) about how they used [simulation testing](https://apple.github.io/foundationdb/testing.html) to test [FoundationDB](https://www.foundationdb.org/) (so well that Kyle "aphyr" Kingsbury didn't feel it was [worth](https://twitter.com/aphyr/status/405017101804396546) Jepsen testing it).

    Watching the talk and rereading the Perlis quote makes one wonder: was the technique independently rediscovered, or had they in fact read the (in)famous 1968 NATO software engineering report?

-   There's also the more established practice of [discrete-event simulation](https://en.wikipedia.org/wiki/Discrete-event_simulation) which is usually used in different contexts than software testing, but nevertheless is close enough in principle that it's worth taking inspiration from (and indeed the simulation testing people often refer to it).

-   Other users of simulation testing include.

    -   AWS

Here's a quote from the recently published paper [Millions of Tiny Databases](https://www.usenix.org/conference/nsdi20/presentation/brooker) (2020) written by three AWS engineers:

"To solve this problem \[testing distributed systems\], we picked an approach that is in wide use at Amazon Web Services, which we would like to see broadly adopted: build a test harness which abstracts networking, performance, and other systems concepts (we call it a simworld). The goal of this approach is to allow developers to write distributed systems tests, including tests that simulate packet loss, server failures, corruption, and other failure cases, as unit tests in the same language as the system itself. In this case, these unit tests run inside the developer's IDE (or with junit at build time), with no need for test clusters or other infrastructure. A typical test which tests correctness under packet loss can be implemented in less than 10 lines of Java code, and executes in less than 100ms. The Physalia team have written hundreds of such tests, far exceeding the coverage that would be practical in any cluster-based or container-based approach.

The key to building a simworld is to build code against abstract physical layers (such as networks, clocks, and disks). In Java we simply wrap these thin layers in interfaces. In production, the code runs against implementations that use real TCP/IP, DNS and other infrastructure. In the simworld, the implementations are based on in-memory implementa- tions that can be trivially created and torn down. In turn, these in-memory implementations include rich fault-injection APIs, which allow test implementors to specify simple statements like: `net.partitionOff ( PARTITION_NAME , p5.getLocalAddress () ); ...   net.healPartition ( PARTITION_NAME );`

Our implementation allows control down to the packet level, allowing testers to delay, duplicate or drop packets based on matching criteria. Similar capabilities are available to test disk IO. Perhaps the most important testing capability in a distributed database is time, where the framework allows each actor to have it's own view of time arbitrarily controlled by the test. Simworld tests can even add Byzantine conditions like data corruption, and operational properties like high la- tency. We highly recommend this testing approach, and have continued to use it for new systems we build."

    + Dropbox

      * https://dropbox.tech/infrastructure/rewriting-the-heart-of-our-sync-engine
      * https://lobste.rs/s/ob6a8z/rewriting_heart_our_sync_engine

    + Basho

     * [Riak](https://speakerdeck.com/jtuple/hansei-property-based-development-of-concurrent-systems)
       (a distributed NoSQL key-value data store that offers high availability, fault
       tolerance, operational simplicity, and scalability)

    + IOHK

From their recent [paper](http://www.cse.chalmers.se/~rjmh/tfp/proceedings/TFP_2020_paper_11.pdf) "Flexibility with Formality: Practical Experience with Agile Formal Methods in Large-Scale Functional Programming" (2020):

"Both the network and consensus layers must make significant use of concurrency which is notoriously hard to get right and to test. We use Software Transactional Memory(STM) to manage the internal state of a node. While STM makes it much easier to write correct concurrent code, it is of course still possible to get wrong, which leads to intermittent failures that are hard to reproduce and debug.

In order to reliably test our code for such concurrency bugs, we wrote a simulator that can execute the concurrent code with both timing determinism and giving global observability, producing execution traces. This enables us to write property tests that can use the execution traces and to run the tests in a deterministic way so that any failures are always reproducible. The use of the mini-protocol design pattern, the encoding of protocol interactions in session types and the use of a timing reproducable simulation has yielded several advantages:

-   Adding new protocols (for new functionality) with strong assurance that they will not interact adversly with existing functionality and/or performance consistency.

-   Consistent approaches (re-usable design approaches) to issues of latency hiding, intra mini-protocol flow control and timeouts / progress criteria.

-   Performance consistent protocol layer abstraction / subsitution: construct real world realistic timing for operation without complexity of simulating all the underlying layer protocol complexity. This helps designs / development to maintain performance target awareness during development.

-   Consitent error propagation and mitigation (mini protocols to a peer live/die together) removing issues of resource lifetime management away from mini-protocol designers / implementors."

The simulation code is open source and can be found [here](https://github.com/input-output-hk/ouroboros-network/tree/master/io-sim).

---

``` {.haskell .literate}
module ATMC.Lec06WhiteboxCheckers where
```

## Motivation

-   Previously we've seen how to black-box test our system, but simulation gives us full insight into what's happening inside "the box" so we can write more interesting white-box checkers

## See also

-   The `detsys` [`ltl`](https://github.com/symbiont-io/detsys-testkit/tree/main/src/ltl) checker
-   [Quickstrom](https://arxiv.org/abs/2203.11532v1)
-   [HasCal](https://github.com/Gabriel439/HasCal)

## Exercises

0.  Can we model check our state machines a la TLC/HasCal while retaining the possibility to simulate and "production deploy" them? (Hint: see the [P](https://github.com/p-org/P) programming language.)

---

``` {.haskell .literate}
module ATMC.Lec07EfficientEventLoop where
```

## Motivation

-   Our event loop is deterministic which allows for simulation, but it's not the most efficient in terms of how fast and how much traffic it can serve

## Plan

-   Pipelining (shared multi-cast queue a la LMAX' disruptor)
-   Journal?

## See also

-   The Linux kernel's [io_uring](https://kernel.dk/io_uring.pdf) interface
-   [aeron](https://github.com/real-logic/aeron)

---

``` {.haskell .literate}
module ATMC.Lec08AsyncFileSystemIO where
```

## Motivation

-   Filesystem I/O is slow and blocks the event loop

## See also

-   [Continuations](https://matt.might.net/articles/programming-with-continuations--exceptions-backtracking-search-threads-generators-coroutines/)
-   [Goblins](https://docs.racket-lang.org/goblins/)
-   [E programming language](https://en.wikipedia.org/wiki/E_(programming_language))

---

``` {.haskell .literate}
module ATMC.Lec09SMUpgrades where
```

# State machine upgrades

## Motivation

-   How do we upgrade the state machines?

## Plan

-   Versioning of all messages
-   Codec upgrades
-   State machine upgrades
-   Seralisation of state machines?

## See also

-   Erlang hot-code swapping

---

``` {.haskell .literate}
module ATMC.Lec10LibraryOrFramework where
```

## Motivation

-   How can we take everything we've done so far and pack it up in a nice way so that many different applications can be written, easily communicate with each other all while being conveniently debuggable?

## See also

-   Jane Street's [Concord](https://signalsandthreads.com/state-machine-replication-and-why-you-should-care/) framework

-   Chuck's Bandwagon framework

---

# Advanced property-based testing mini-course

-   Goals:

    -   Show how to test stateful (i.e. impure/monadic) programs using property-based testing in general;
    -   Show how to use fault injection and so called simulation testing to test distributed systems in particular;
    -   Introduce the reader to related work and open problems in the area.

-   Pre-requisites:

    -   Enough familiarity with Haskell to be able to read simple programs, for example if you can follow along in the *Learn You a Haskell for Great Good!* [tutorial](http://learnyouahaskell.com/chapters), then you should be fine;

    -   Basic knowledge of state machines (i.e. [Mealy](https://en.wikipedia.org/wiki/Mealy_machine) / [Moore machines](https://en.wikipedia.org/wiki/Moore_machine) and [transducers](https://en.wikipedia.org/wiki/Finite-state_transducer)).

    -   Some experience with property-based testing of non-stateful (i.e. pure) programs. For example as explained in the official QuickCheck [manual](http://www.cse.chalmers.se/~rjmh/QuickCheck/manual.html) or in the following [tutorial](https://begriffs.com/posts/2017-01-14-design-use-quickcheck.html).

## Structure

Each lecture has the following structure:

-   Motiviation: explains why we are doing what we are about to do;
-   Plan: how we will do it;
-   Code: an implementation of the idea;
-   Discussion: common questions or objections;
-   Exercises: things the authors were to lazy to do, but they know how to;
-   Problems: things the authors don't know how to do (yet);
-   See also: links to further reading about the topic or related topics;
-   Summary: the most important take away.

The lectures build upon each other. We start by modelling and testing a simple counter using a state machine in lecture 1, we then reuse the same state machine model to test the counter of thread-safety using linearisability in lecture 2. In lecture 3 we will implement a queue and a web service that uses said queue, the state machine model for the queue and the real implementation of the queue will be contract tested to ensure that the model is faithful to the implementation, subsequently while testing the web service we will use the model in place of the real queue. In lecture 4 we introduce fault injection to the queue allowing us to test how the web service performs when its dependency fails. Finally, in lecture 5, we combine all the above ideas in what, sometimes is called simulation testing, to test a distributed system that uses replicated state machines.

## Table of contents

1.  State machine testing

-   State machine models
-   Pre-conditions
-   Coverage
-   Execution trace for counterexamples
-   Regression tests from counterexamples
-   Metrics
-   References?

2.  Concurrent state machine testing with linearisability

-   Generalise generation and execution to N threads
-   Collect history
-   Enumerate all possible sequential executions from concurrent history
-   Write simple linearisability checker: check if there's any such sequential execution that satisifies the (sequential) state machine model

3.  Consumer-driven contract tests using state machines
4.  Fault-injection
5.  Simulation testing

``` {.haskell .literate}
module ATMC where
```

``` {.haskell .literate}
import ATMC.Lec01SMTesting
import ATMC.Lec02ConcurrentSMTesting
import ATMC.Lec03SMContractTesting
import ATMC.Lec04FaultInjection
import ATMC.Lec05SimulationTesting
```

---

