Fault-injection
===============

Motivation
----------

- "almost all (92%) of the catastrophic system failures are the result of
  incorrect handling of non-fatal errors explicitly signaled in software. [...]
  in 58% of the catastrophic failures, the underlying faults could easily have
  been detected through simple testing of error handling code." -- [Simple
  Testing Can Prevent Most Critical Failures: An Analysis of Production Failures
  in Distributed Data-intensive
  Systems](http://www.eecg.toronto.edu/~yuan/papers/failure_analysis_osdi14.pdf)
  (2014) Yuan et al.

- "[Jepsen](https://jepsen.io/analyses) has analyzed over two dozen databases,
  coordination services, and queues -- and we've found replica divergence, data
  loss, stale reads, read skew, lock conflicts, and much more."

Plan
----

- Create a wrapper around our fake queue implementation which allows us to
  inject faults

- Possible faults to inject for the queue
   + write fails, e.g. queue is full
   + read fails, e.g. queue is empty
   + read crashes, e.g. bug in queue causes exception to be thrown
   + read returns a malformed write which no longer deserialises, or has an
     invalid client request id to send the response to

- Use the same web service model that we wrote in the previous lecture and do
  integration/"collaboration" tests with the fake queue with faults to ensure
  that our web service, that uses the queue, doesn't crash even if the presence
  of faults in the queue

- Notice that we don't have to make any changes to the model to account of the
  possible faults, the linearisability checker does this for us behind the
  scenes

How it works
------------

XXX:

- How faults get injected
- How linearisability checker deals with fails and infos
- How we generate faults

XXX:

Code
----

> {-# LANGUAGE OverloadedStrings #-}
> {-# LANGUAGE NumericUnderscores #-}
> {-# LANGUAGE ScopedTypeVariables #-}

> module Lec04FaultInjection
>   ( module Lec04FaultInjection
>   , module Lec03.ServiceTest
>   , module Test.QuickCheck
>   )
>   where

> import Control.Monad (replicateM_)
> import Control.Concurrent (threadDelay, myThreadId)
> import Control.Concurrent.Async (mapConcurrently)
> import Control.Concurrent.STM (TQueue, atomically, flushTQueue, newTQueueIO)
> import Data.Vector (Vector)
> import qualified Data.Vector as Vector
> import Data.ByteString.Lazy (ByteString)
> import qualified Data.ByteString.Lazy as LBS
> import Control.Exception
> import Control.Monad.IO.Class (MonadIO, liftIO)
> import Data.IORef
> import Data.List (permutations)
> import Test.HUnit (Assertion, assertBool)
> import Test.QuickCheck hiding (Result)
> import Test.QuickCheck.Monadic hiding (assert)
> import Network.HTTP.Types (status503)
> import Network.HTTP.Client (HttpException(HttpExceptionRequest),
>                             HttpExceptionContent(StatusCodeException), Manager,
>                             defaultManagerSettings, newManager, responseStatus,
>                             managerResponseTimeout, responseTimeoutMicro)

We have to generalise the notion of histories and linearisability from the
second lecture to deal with faults, but otherwise the idea is the same.

> import Lec02ConcurrentSMTesting (assertWithFail, classifyCommandsLength, toPid)
> import Lec03.Service (withService, mAX_QUEUE_SIZE, fakeQueue, realQueue)
> import Lec03.QueueInterface
> import Lec03.ServiceTest (Index(Index), httpWrite, httpRead, httpReset)
> import Lec04.LineariseWithFault (History'(History), Operation'(..), FailureMode(..), interleavings,
>                                  linearisable, prettyHistory, appendHistory)

A queue which supports fault-injection is merely a queue and a mutable variable
that might contain a fault.

> data FaultyFakeQueue a = FaultyFakeQueue
>   { ffqQueue :: QueueI a
>   , ffqFault :: IORef (Maybe Fault)
>   }

The possible faults that we support are that the queue is full (write fails),
that its empty (read fails), read throwing an exception (due to some
implementation bug perhaps), and slow read (simulating a GC or I/O pause).

> data Fault = Full | Empty | ReadFail IOException | ReadSlow
>   deriving Show

We can now create a wrapper around our fake queue which inserts the right
failure points depending which fault is injected. Note that a fault is only
active in until it's triggered.

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
>         Just Full -> do
>           removeFault ref
>           return False
>         _otherwise -> qiEnqueue fake x

>     dequeue :: QueueI a -> IORef (Maybe Fault) -> IO (Maybe a)
>     dequeue fake ref = do
>       fault <- readIORef ref
>       case fault of
>         Just Empty -> do
>           removeFault ref
>           return Nothing
>         Just (ReadFail err) -> do
>           removeFault ref
>           throwIO err
>         Just ReadSlow -> do
>           removeFault ref
>           threadDelay 200_000 -- 0.2s
>           qiDequeue fake
>         _otherwise -> qiDequeue fake

To make it a bit easier to work with fault-injection we introduce the following
helper functions.

> injectFullFault :: IORef (Maybe Fault) -> IO ()
> injectFullFault ref = writeIORef ref (Just Full)

> injectEmptyFault :: IORef (Maybe Fault) -> IO ()
> injectEmptyFault ref = writeIORef ref (Just Empty)

> injectReadFailFault :: IORef (Maybe Fault) -> IOException -> IO ()
> injectReadFailFault ref err = writeIORef ref (Just (ReadFail err))

> injectReadSlowFault :: IORef (Maybe Fault) -> IO ()
> injectReadSlowFault ref = writeIORef ref (Just ReadSlow)

> removeFault :: IORef (Maybe Fault) -> IO ()
> removeFault ref = writeIORef ref Nothing

The following unit test should give an idea of how the fake queue with support
for fault-injection works.

> test_injectFullFault :: IO ()
> test_injectFullFault = do
>   ffq <- faultyFakeQueue 4
>   res1 <- qiEnqueue (ffqQueue ffq) ("test1" :: String)
>   assert (res1 == True) (return ())
>   injectFullFault (ffqFault ffq)
>   res2 <- qiEnqueue (ffqQueue ffq) "test2"
>   assert (res2 == False) (return ())

Sequential integration/"collaboration" testing
----------------------------------------------

The notion of program needs to be generalised to include fault-injection, but
otherwise it should look familiar.

> data Command
>   = ClientRequest ClientRequest
>   | InjectFault Fault
>   | Reset -- Used for testing only.
>   deriving Show

> data ClientRequest = WriteReq ByteString | ReadReq Index
>   deriving Show

> data ClientResponse = WriteResp Index | ReadResp ByteString
>   deriving (Eq, Show)

> newtype Program = Program { unProgram :: [Command] }
>   deriving Show

> genProgram :: Model -> Gen Program
> genProgram m0 = sized (go m0 [])
>   where
>     go _m cmds 0 = return (Program (reverse cmds))
>     go  m cmds n = do
>       cmd <- genCommand m
>       case mMaybeFault m of
>         Nothing -> do
>           let m' = fst (step m cmd)
>           go m' (cmd : cmds) (n - 1)
>         Just _fault -> go m (cmd : cmds) (n - 1)

> genCommand :: Model -> Gen Command
> genCommand m0 = case mMaybeFault m0 of
>   Nothing -> frequency [ (1, InjectFault <$> genFault)
>                        , (9, ClientRequest <$> genRequest m0)
>                        ]
>   Just _fault -> ClientRequest <$> genRequest m0
>   where
>     genRequest :: Model -> Gen ClientRequest
>     genRequest m | len == 0  = WriteReq <$> (LBS.pack <$> arbitrary)
>                  | otherwise = frequency
>                      [ (2, WriteReq <$> (LBS.pack <$> arbitrary))
>                      , (8, ReadReq  <$> (Index <$> elements [0 .. len - 1]))
>                      ]
>       where
>         len = Vector.length (mModel m)

>     genFault :: Gen Fault
>     genFault = frequency [ (1, pure Full)
>                          , (1, pure Empty)
>                          , (1, pure (ReadFail (userError "bug")))
>                          , (1, pure ReadSlow)
>                          ]

> data Model = Model
>   { mModel      :: Vector ByteString
>   , mMaybeFault :: Maybe Fault
>   }

> initModel :: Model
> initModel = Model Vector.empty Nothing

> step :: Model -> Command -> (Model, Maybe ClientResponse)
> step m cmd = case cmd of
>   ClientRequest (WriteReq bs) ->
>     ( m { mModel = Vector.snoc (mModel m) bs, mMaybeFault = Nothing }
>     , Just (WriteResp (Index (Vector.length (mModel m))))
>     )
>   ClientRequest (ReadReq (Index ix)) ->
>     ( m { mMaybeFault = Nothing }
>     , ReadResp <$> mModel m Vector.!? ix
>     )
>   InjectFault fault -> (m { mMaybeFault = Just fault}, Nothing)
>   Reset             -> (m, Nothing)

> data Result a = ROk a | RFail | RInfo | RNemesis
>   deriving Show

> exec :: Command -> IORef (Maybe Fault) -> Manager -> IO (Result ClientResponse)
> exec (ClientRequest req) _ref mgr =
>   case req of
>     WriteReq bs -> do
>       res <- try (httpWrite mgr bs)
>       case res of
>         Left (err :: HttpException) | is503 err -> return RFail
>                                     | otherwise -> return RInfo
>         Right ix -> return (ROk (WriteResp ix))
>     ReadReq ix  -> do
>       res <- try (httpRead mgr ix)
>       case res of
>         -- NOTE: since read doesn't change the state we can always treat is a failure.
>         Left (_err :: HttpException) -> return RFail
>         Right bs -> return (ROk (ReadResp bs))
>     where
>       is503 :: HttpException -> Bool
>       is503 (HttpExceptionRequest _req (StatusCodeException resp _bs)) = responseStatus resp == status503
>       is503 _otherwise = False
> exec (InjectFault fault)  ref _mgr = do
>   case fault of
>     Full         -> injectFullFault ref
>     Empty        -> injectEmptyFault ref
>     ReadFail err -> injectReadFailFault ref err
>     ReadSlow     -> injectReadSlowFault ref
>   return RNemesis
> exec Reset _ref mgr = do
>   httpReset mgr
>   return RNemesis

> shrinkProgram :: Program -> [Program]
> shrinkProgram (Program cmds) = filter (isValidProgram initModel) ((map Program (shrinkList shrinkCommand cmds)))
>   where
>     shrinkCommand _cmd = []

> isValidProgram :: Model -> Program -> Bool
> isValidProgram m0 (Program cmds0) = go m0 cmds0
>   where
>     go _m [] = True
>     go  m (ClientRequest (ReadReq (Index ix)) : cmds)
>       | ix < Vector.length (mModel m) = go m cmds
>       | otherwise                     = False
>     go  m (cmd@(ClientRequest (WriteReq _bs)) : cmds) = case mMaybeFault m of
>       Nothing     -> let m' = fst (step m cmd) in go m' cmds
>       Just _fault -> go m cmds
>     go  m (cmd : cmds) = let m' = fst (step m cmd) in go m' cmds

> forallPrograms :: (Program -> Property) -> Property
> forallPrograms p =
>   forAllShrink (genProgram initModel) shrinkProgram p

> prop_sequentialWithFaults :: IORef (Maybe Fault) -> Manager -> Property
> prop_sequentialWithFaults ref mgr = forallPrograms $ \prog -> monadicIO $ do
>   r <- runProgram ref mgr initModel prog
>   run (removeFault ref)
>   run (httpReset mgr)
>   case r of
>     Left err -> do
>       monitor (counterexample err)
>       return False
>     Right () -> return True

> runProgram :: MonadIO m => IORef (Maybe Fault) -> Manager -> Model -> Program -> m (Either String ())
> runProgram ref mgr m0 (Program cmds0) = go m0 cmds0
>   where
>     go _m []           = return (Right ())
>     go  m (cmd : cmds) = do
>       res <- liftIO (exec cmd ref mgr)
>       case res of
>         ROk resp -> do
>           let (m', mResp') = step m cmd
>           case mResp' of
>             Just resp' | resp == resp' -> go m' cmds
>                        | otherwise     -> return (Left (concat [show resp, " /= ", show resp']))
>             Nothing -> return (Left (concat [show res, " /= ", show mResp']))
>         RFail -> go m cmds
>         -- For more see the "Crashes" section of https://jepsen.io/consistency
>         RInfo -> return (Left "Continuing would violate the single-threaded constraint: processes only do one thing at a time.")
>         RNemesis -> do
>           let (m', mResp') = step m cmd
>           case mResp' of
>             Nothing     -> go m' cmds
>             Just _resp' -> return (Left (concat [show res, " /= ", show mResp']))

> withFaultyQueueService :: (Manager -> IORef (Maybe Fault) -> IO ()) -> IO ()
> withFaultyQueueService io = do
>   queue <- faultyFakeQueue mAX_QUEUE_SIZE
>   mgr   <- newManager defaultManagerSettings
>              { managerResponseTimeout = responseTimeoutMicro (10_000_000) } -- 10s
>   withService (ffqQueue queue) (io mgr (ffqFault queue))

> withFakeQueueService :: (Manager -> IO ()) -> IO ()
> withFakeQueueService io = do
>   queue <- fakeQueue mAX_QUEUE_SIZE
>   mgr   <- newManager defaultManagerSettings
>   withService queue (io mgr)

> withRealQueueService :: (Manager -> IO ()) -> IO ()
> withRealQueueService io = do
>   queue <- realQueue mAX_QUEUE_SIZE
>   mgr   <- newManager defaultManagerSettings
>   withService queue (io mgr)

> unit_faultTest :: IO ()
> unit_faultTest =
>   withFaultyQueueService (\mgr ref -> quickCheck (prop_sequentialWithFaults ref mgr))

> assertProgram :: String -> Program -> Assertion
> assertProgram msg prog =
>   withFaultyQueueService $ \mgr ref -> do
>     let m = initModel
>     r <- runProgram ref mgr m prog
>     assertBool msg (isRight r)
>   where
>     isRight Right {} = True
>     isRight Left  {} = False


Concurrent integration/"collaboration" testing
----------------------------------------------

> newtype ConcProgram = ConcProgram { unConcProgram :: [[Command]] }
>   deriving Show

> forAllConcProgram :: (ConcProgram -> Property) -> Property
> forAllConcProgram k =
>   forAllShrinkShow (genConcProgram initModel) (shrinkConcProgram initModel)
>                    prettyConcProgram k
>   where

>     genConcProgram :: Model -> Gen ConcProgram
>     genConcProgram m0 = sized (go m0 [])
>       where
>         go :: Model -> [[Command]] -> Int -> Gen ConcProgram
>         go m acc sz | sz <= 0   = return (ConcProgram (reverse acc))
>                     | otherwise = do
>                         n <- chooseInt (2, 3)
>                         cmds <- vectorOf n (genCommand m) `suchThat` concSafe m
>                         go (advanceModel m cmds) (cmds : acc) (sz - n)

>     advanceModel :: Model -> [Command] -> Model
>     advanceModel m cmds = foldl (\ih cmd -> fst (step ih cmd)) m cmds

>     concSafe :: Model -> [Command] -> Bool
>     concSafe m = all (isValidProgram m . Program) . permutations

>     validConcProgram :: Model -> ConcProgram -> Bool
>     validConcProgram m0 (ConcProgram cmdss0) = go m0 True cmdss0
>       where
>         go :: Model -> Bool -> [[Command]] -> Bool
>         go _m False _              = False
>         go _m acc   []             = acc
>         go  m _acc  (cmds : cmdss) = go (advanceModel m cmds) (concSafe m cmds) cmdss

>     shrinkConcProgram :: Model -> ConcProgram -> [ConcProgram]
>     shrinkConcProgram m
>       = filter (validConcProgram m)
>       . map ConcProgram
>       . filter (not . null)
>       . shrinkList (shrinkList shrinkCommand)
>       . unConcProgram
>       where
>         shrinkCommand _cmd = []

>     prettyConcProgram :: ConcProgram -> String
>     prettyConcProgram = show

> type Operation = Operation' Command (Maybe ClientResponse)

> concExec :: IORef (Maybe Fault) -> Manager -> TQueue Operation -> Command -> IO ()
> concExec ref mgr hist cmd = do
>   pid <- toPid <$> myThreadId -- XXX: we can't reuse crashed pids...
>   appendHistory hist (Invoke pid cmd)
>   res <- exec cmd ref mgr
>   case res of
>     ROk resp -> appendHistory hist (Ok pid (Just resp))
>     RFail    -> appendHistory hist (Fail pid FAIL)
>     RInfo    -> appendHistory hist (Fail pid INFO)
>     RNemesis -> appendHistory hist (Ok pid Nothing)

> -- NOTE: Assumes that the service is running.
> prop_faultyCollaborationTests :: IORef (Maybe Fault) -> Manager -> Property
> prop_faultyCollaborationTests ref mgr = mapSize (min 20) $
>   forAllConcProgram $ \(ConcProgram cmdss) -> monadicIO $ do
>     monitor (classifyCommandsLength (concat cmdss))
>     monitor (tabulate "Client requests" (map constructorString (concat cmdss)))
>     monitor (tabulate "Number of concurrent client requests" (map (show . length) cmdss))
>     -- Rerun a couple of times, to avoid being lucky with the interleavings.
>     replicateM_ 10 $ do
>       history <- run newTQueueIO
>       run (mapM_ (mapConcurrently (concExec ref mgr history)) cmdss)
>       hist <- History <$> run (atomically (flushTQueue history))
>       assertWithFail (linearisable step initModel (interleavings hist)) (prettyHistory hist)
>       run (removeFault ref)
>       run (httpReset mgr)
>   where
>     constructorString :: Command -> String
>     constructorString (ClientRequest WriteReq {}) = "WriteReq"
>     constructorString (ClientRequest ReadReq  {}) = "ReadReq"
>     constructorString (InjectFault Full)          = "Full"
>     constructorString (InjectFault Empty)         = "Empty"
>     constructorString (InjectFault ReadFail {})   = "ReadFail"
>     constructorString (InjectFault ReadSlow {})   = "ReadSlow"
>     constructorString Reset                       = "Reset"

> test :: IO ()
> test = do
>   -- NOTE: fake queue is used here, justified by previous contract testing.
>   ffq <- faultyFakeQueue mAX_QUEUE_SIZE
>   mgr <- newManager defaultManagerSettings
>   withService (ffqQueue ffq) (quickCheck (prop_faultyCollaborationTests (ffqFault ffq) mgr))


Discussion
----------

- Faults baked into `Command` or separate?

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

1. Faults in the real world are often correlated with each other, i.e. if one
   thing fails it's likely it will overload some other thing making it more
   likely to fail and so on, how can we simulate this in a good way?

2. What's better at finding bugs: i) a small fixed agenda and try many faults
   and interleavings, or ii) a big random agenda and fewer interleavings?

See also
--------

- [*Why Is Random Testing Effective for Partition Tolerance
  Bugs?*(https://dl.acm.org/doi/pdf/10.1145/3158134) by Majumdar and Niksic
  (2018)

- The [`failpoint`](https://github.com/pingcap/failpoint) Go library allows us
  to insert failures at any point in our code, in some way this is a
  cheap-and-dirty way of achiving the same thing as we've done in this lecture.
  Cheap because it's less work, dirty because it litters the (production) code
  with fail points (our fail points are hidden in the fake).

Summary
-------

- Most catastrophic system failures are related to error handling,
  fault-injection allows us to trigger error handling code and therefore
  increase our coverage to catch these failures before they happen in
  production;

- Fault-injection is much easier to do determinstically when using fakes.
