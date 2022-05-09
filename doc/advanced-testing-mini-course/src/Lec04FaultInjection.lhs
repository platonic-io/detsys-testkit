> module Lec04FaultInjection where

> import Data.Vector (Vector)
> import qualified Data.Vector as Vector
> import Data.ByteString.Lazy (ByteString)
> import Control.Exception
> import Control.Monad.IO.Class (MonadIO, liftIO)
> import Data.IORef
> import Test.QuickCheck
> import Test.QuickCheck.Monadic hiding (assert)
> import Network.HTTP.Client (Manager)

> import Lec04.LineariseWithFault

Fault-injection
===============

Motivation
----------

  - "almost all (92%) of the catastrophic system failures are the result of
    incorrect handling of non-fatal errors explicitly signaled in software.
    [...] in 58% of the catastrophic failures, the underlying faults could
    easily have been detected through simple testing of error handling code." --
    [Simple Testing Can Prevent Most Critical Failures: An Analysis of
    Production Failures in Distributed Data-intensive
    Systems](http://www.eecg.toronto.edu/~yuan/papers/failure_analysis_osdi14.pdf)
    (2014) Yuan et al;

Plan
----

- Create a wrapper around our fake queue implementation which allows us to inject faults

- Possible faults to inject for the queue
   + write fails, e.g. queue is full
   + read fails, e.g. queue is empty
   + read crashes, e.g. bug in queue causes exception to be thrown
   + read returns a malformed write which no longer deserialises, or has a valid
     client request id to send the response to

- Use the same web service model that we write in the previous lecture and do
  "collaboration tests" with the fake queue with faults to ensure that our
  service that uses the queue doesn't crash even if the presence of faults

- Notice that we don't have to make any changes to the model to account of the
  possible faults, the linearisability checker does this for us behind the
  scenes

Faulty queue
------------

> import Lec03.QueueInterface
> import Lec03.Service (fakeQueue)
> import Lec03.ServiceTest (Index(Index), httpWrite, httpRead, httpReset)

> data FaultyFakeQueue a = FaultyFakeQueue
>   { ffqQueue :: QueueI a
>   , ffqFault :: IORef (Maybe Fault)
>   }

> data Fault = Full | Empty | ReadFail IOException
>   deriving Show

> faultyFakeQueue :: Int -> IO (FaultyFakeQueue a)
> faultyFakeQueue size = do
>   fake <- fakeQueue size
>   ref  <- newIORef Nothing
>   return FaultyFakeQueue
>     { ffqQueue = QueueI
>         { qiEnqueue = enqueue fake ref
>         , qiDequeue = dequeue fake ref
>         }
>     , ffqFault = ref
>     }
>   where
>     enqueue :: QueueI a -> IORef (Maybe Fault) -> a -> IO Bool
>     enqueue fake ref x = do
>       fault <- readIORef ref
>       case fault of
>         Just Full  -> return False
>         _otherwise -> qiEnqueue fake x

>     dequeue :: QueueI a -> IORef (Maybe Fault) -> IO (Maybe a)
>     dequeue fake ref = do
>       fault <- readIORef ref
>       case fault of
>         Just Empty          -> return Nothing
>         Just (ReadFail err) -> throwIO err
>         _otherwise          -> qiDequeue fake

> injectFullFault :: FaultyFakeQueue a -> IO ()
> injectFullFault (FaultyFakeQueue _queue ref) = writeIORef ref (Just Full)

> injectEmptyFault :: FaultyFakeQueue a -> IO ()
> injectEmptyFault (FaultyFakeQueue _queue ref) = writeIORef ref (Just Empty)

> injectReadFailFault :: FaultyFakeQueue a -> IOException -> IO ()
> injectReadFailFault (FaultyFakeQueue _queue ref) err = writeIORef ref (Just (ReadFail err))

> removeFault :: FaultyFakeQueue a -> IO ()
> removeFault (FaultyFakeQueue _queue ref) = writeIORef ref Nothing

> test_injectFullFault :: IO ()
> test_injectFullFault = do
>   ffq <- faultyFakeQueue 4
>   res1 <- qiEnqueue (ffqQueue ffq) "test1"
>   assert (res1 == True) (return ())
>   injectFullFault ffq
>   res2 <- qiEnqueue (ffqQueue ffq) "test2"
>   assert (res2 == False) (return ())

Model
-----

> data Command
>   = ClientRequest ClientRequest
>   | InjectFault Fault
>   | RemoveFault
>   | Reset -- Used for testing only.
>   deriving Show

> data ClientRequest = WriteReq ByteString | ReadReq Index
>   deriving Show

> data ClientResponse = WriteResp Index | ReadResp (Maybe ByteString)
>   deriving (Eq, Show)

> newtype Program = Program { unProgram :: [Command] }
>   deriving Show

> genProgram :: Model -> Gen Program
> genProgram (Model vec mFault) = undefined

> data Model = Model
>   { mModel      :: Vector ByteString
>   , mMaybeFault :: Maybe Fault
>   }

> step :: Model -> Command -> (Model, Maybe ClientResponse)
> step m cmd = case cmd of
>   ClientRequest (WriteReq bs) ->
>     ( m { mModel = Vector.snoc (mModel m) bs }
>     , Just (WriteResp (Index (Vector.length (mModel m))))
>     )
>   ClientRequest (ReadReq (Index ix)) -> (m, Just (ReadResp (mModel m Vector.!? ix)))
>   InjectFault fault -> (m { mMaybeFault = Just fault}, Nothing)
>   RemoveFault       -> (m { mMaybeFault = Nothing}, Nothing)
>   Reset             -> (m, Nothing)

> exec :: Command -> FaultyFakeQueue ClientRequest -> Manager -> IO (Maybe ClientResponse)
> exec (ClientRequest req) _queue mgr =
>   case req of
>     WriteReq bs -> Just . WriteResp <$> httpWrite mgr bs
>     ReadReq ix  -> Just . ReadResp  <$> httpRead mgr ix
> exec (InjectFault fault)  queue _mgr = do
>   case fault of
>     Full         -> injectFullFault queue
>     Empty        -> injectEmptyFault queue
>     ReadFail err -> injectReadFailFault queue err
>   return Nothing
> exec RemoveFault queue _mgr = do
>   removeFault queue
>   return Nothing
> exec Reset _queue mgr = do
>   httpReset mgr
>   return Nothing

> shrinkProgram :: Program -> [Program]
> shrinkProgram _prog = []

> forallPrograms :: (Program -> Property) -> Property
> forallPrograms p =
>   forAllShrink (genProgram initModel) shrinkProgram p

> prop_sequentialWithFaults :: FaultyFakeQueue ClientRequest -> Manager -> Property
> prop_sequentialWithFaults queue mgr = forallPrograms $ \prog -> monadicIO $ do
>   runProgram queue mgr initModel prog

> runProgram :: MonadIO m => FaultyFakeQueue ClientRequest -> Manager -> Model -> Program -> m Bool
> runProgram q mgr m0 (Program cmds0) = go m0 cmds0
>   where
>      go _m []           = return True
>      go  m (cmd : cmds) = do
>        resp <- liftIO (exec cmd q mgr)
>        let (m', resp') = step m cmd
>        if resp == resp'
>        then go m' cmds
>        else return False

> initModel :: Model
> initModel = Model Vector.empty Nothing

> newtype ConcProgram = ConcProgram { unConcProgram :: [[Command]] }
>   deriving Show



-- XXX: make `exec` :: Command -> IO {Fail, Info, Ok Response}

-- XXX: stop testing if sequential property hits Info, otherwise we break:

--     Crashes
--
--     If an operation does not complete for some reason (perhaps because it
--     timed out or a critical component crashed) that operation has no
--     completion time, and must, in general, be considered concurrent with
--     every operation after its invocation. It may or may not execute.
--
--     A process with an operation is in this state is effectively stuck, and
--     can never invoke another operation again. If it were to invoke another
--     operation, it would violate our single-threaded constraint: processes
--     only do one thing at a time.
--
-- Source: https://jepsen.io/consistency

Discussion
----------

- Modelling the faults, i.e. move some non-determinism out from linearisability
  checker into the model, is possible but not recommended as it complicated the
  model.

- Can we not just inject real faults like Jepsen does?
  [`iptables`](https://linux.die.net/man/8/iptables) for dropping messages and
  network partitions, [`tc`](https://man7.org/linux/man-pages/man8/tc.8.html) for
  creating latency or simulating a slow connection on the network,
  [`(p)kill`](https://linux.die.net/man/1/kill) for killing processes, `kill -STOP
  $pid` and `kill -CONT $pid` for pausing and resuming processes to simulate long
  I/O or GC pauses, [`libfaketime`](https://github.com/wolfcw/libfaketime) for
  clock-skews, etc?

  We could, after all Jepsen is a very successful at finding bugs in distributed
  databases using these techniques. However keep in mind exactly how Jepsen is
  used: typically companies hire Kyle Kingsbury for a couple of weeks/months, he
  writes the tests and runs them, analyses the results and writes a report.

  XXX:

  * requires root, needs to be done in containers or vm which slows down and
    complicates start up
  * imprecise (e.g. `iptables` can't drop exactly the 42nd message and only if it's a read)
  * non-deterministic
  * slow (we need to wait for timeouts to happend, ~30-90 secs)
  * ci flakiness (e.g. `docker pull` failing)
  * blackbox

- Can we contract test the fault injection? I.e. how do we know that the faults
  we inject correspond to real faults that can happen? How can we be sure to
  have covered all possible real faults?

  To answer questions of this kind it helps to specify fault models, for an
  example of this see `tigerbeetle`'s
  [documentation](https://github.com/coilhq/tigerbeetle/blob/main/docs/DESIGN.md#fault-models),
  one then manually needs to convince oneself of the fact that the fault models
  are covered by the fault injection.

- What about [Chaos engineering](https://en.wikipedia.org/wiki/Chaos_engineering)?

  + Chaos engineering has the same downsides as Jepsen when it comes to being
    slow and non-deterministic

  + It's important to remember in which context it was developed: Netflix
    (hundreds(?) of already designed and deployed systems spanning datacentres
    around the globe), unless you are in that same situation then the fault
    injection techniques discussed here are far simpler to implement

  + Works at a different level, e.g. "over 5% of the traffic receives 500
    errors", rather than "assertion A failed at line number L", i.e. test
    failures will pin-point you much more precisely to where the problem is

  + Tests production configurations, as well as monitoring and alerting

  + In conclusion: chaos engineering is complementary to what we discribed here,
    but probably less bang for the buck and should be done later -- remember the
    quote from the motivation: "[...] in 58% of the catastrophic failures, the
    underlying faults could easily have been detected through simple testing of
    error handling code."

Exercises
---------

0. Try to imagine how much more difficult it would be to write these tests
   without injecting the faults in the fake, but rather the real dependency.

Problems
--------

0. Can we do better than randomly inserting faults? (Hint: see [*Lineage-driven
   Fault Injection*](https://people.ucsc.edu/~palvaro/molly.pdf) by Alvaro et al
   (2015) and the
   [`ldfi`](https://github.com/symbiont-io/detsys-testkit/tree/main/src/ldfi)
   directory in the `detsys-testkit` repo)

See also
--------

- [*Why Is Random Testing Effective for Partition Tolerance
  Bugs?*(https://dl.acm.org/doi/pdf/10.1145/3158134) by Majumdar and Niksic
  (2018)
