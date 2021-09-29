{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}

module StuntDouble.ActorMap where

import Control.Concurrent
import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Exception
import Control.Monad
import Data.Foldable (toList)
import Data.Heap (Entry(Entry), Heap)
import qualified Data.Heap as Heap
import Data.List
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (catMaybes)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.String (fromString)
import Data.Text (Text)
import Data.Time (UTCTime)
import qualified Data.Time as Time
import Data.Typeable
import Data.Vector (Vector)
import qualified Data.Vector as Vector
import System.Exit (exitSuccess)
import System.Random

import StuntDouble.AdminTransport
import StuntDouble.AdminTransport.NamedPipe
import StuntDouble.Codec
import StuntDouble.Envelope
import StuntDouble.FreeMonad
import StuntDouble.Histogram
import StuntDouble.IO
import StuntDouble.Log
import StuntDouble.LogicalTime
import StuntDouble.Message
import StuntDouble.Metrics
import StuntDouble.Random
import StuntDouble.Reference
import StuntDouble.Time
import StuntDouble.Transport
import StuntDouble.Transport.Http
import StuntDouble.Transport.HttpSync
import StuntDouble.Transport.NamedPipe
import StuntDouble.Transport.Stm

------------------------------------------------------------------------

newtype Promise = Promise Int
  deriving (Eq, Ord, Num, Show)

unPromise :: Promise -> Int
unPromise (Promise i) = i

newtype Actor s = Actor { unActor :: Free (ActorF s) Message }

-- XXX: Should we always force the user to supply an exception continuation?
data Resolution
  = TimeoutR
  | TimerR
  | IOResultR IOResult
  | InternalMessageR Message
  | ExceptionR SomeException
  deriving Show

data ActorF s x
  = Invoke LocalRef Message (Message -> x)
  | Send RemoteRef Message (Promise -> x)
  | AsyncIO IOOp (Promise -> x)
  | Typeable s => On Promise (Resolution -> Free (ActorF s) ()) (() -> x)
  | Typeable s => Get (s -> x)
  | Typeable s => Put s (() -> x)
  | GetTime (UTCTime -> x)
  | Random (Double -> x)
  | SetTimer Time.NominalDiffTime (Promise -> x)
  | ClientResponse ClientRef Message (() -> x)
  | DumpLog (Log -> x)
  -- XXX: Log?
  -- XXX: Throw?
deriving instance Functor (ActorF s)

invoke :: LocalRef -> Message -> Free (ActorF s) Message
invoke lref msg = Free (Invoke lref msg return)

send :: RemoteRef -> Message -> Free (ActorF s) Promise
send rref msg = Free (Send rref msg return)

asyncIO :: IOOp -> Free (ActorF s) Promise
asyncIO io = Free (AsyncIO io return)

on :: Typeable s => Promise -> (Resolution -> Free (ActorF s) ()) -> Free (ActorF s) ()
on p k = Free (On p k return)

get :: Typeable s => Free (ActorF s) s
get = Free (Get return)

put :: Typeable s => s -> Free (ActorF s) ()
put s' = Free (Put s' return)

modify :: Typeable s => (s -> s) -> Free (ActorF s) ()
modify f = put . f =<< get

modifys :: Typeable s => (s -> (s, a)) -> Free (ActorF s) a
modifys f = do
  s <- get
  let (s', x) = f s
  put s'
  return x

getTime :: Free (ActorF s) UTCTime
getTime = Free (GetTime return)

random :: Free (ActorF s) Double
random = Free (Random return)

setTimer :: Time.NominalDiffTime -> Free (ActorF s) Promise
setTimer ndt = Free (SetTimer ndt return)

clientResponse :: ClientRef -> Message -> Free (ActorF s) ()
clientResponse cref msg = Free (ClientResponse cref msg return)

dumpLog :: Free (ActorF s) Log
dumpLog = Free (DumpLog return)

------------------------------------------------------------------------

newtype ActorMap = ActorMap (Map LocalRef ActorData)

data ActorData = forall s. Typeable s => ActorData
  { adActor :: Message -> Actor s
  , adState :: s
  , adTime  :: Time -- XXX: Nothing uses this?
  }

emptyActorMap :: ActorMap
emptyActorMap = ActorMap Map.empty

actorMapLookup :: LocalRef -> ActorMap -> Maybe ActorData
actorMapLookup lref (ActorMap m) = Map.lookup lref m

actorMapUnsafeLookup :: LocalRef -> ActorMap -> ActorData
actorMapUnsafeLookup lref am = case actorMapLookup lref am of
  Nothing -> error ("actorMapUnsafeLookup: `" ++ show lref ++ "' not in actor map.")
  Just v  -> v

actorMapUpdateState :: forall s. Typeable s => LocalRef -> s -> ActorMap -> ActorMap
actorMapUpdateState lref s' (ActorMap m) = ActorMap (Map.adjust f lref m)
  where
    f (ActorData a _s t) =
      case cast s' of
        Just s -> ActorData a s t
        Nothing -> error "actorMapUpdateState: impossible, wrong type of state"

actorMapSpawn :: Typeable s => (Message -> Actor s) -> s -> Time -> ActorMap
              -> (LocalRef, ActorMap)
actorMapSpawn a s t (ActorMap m) =
  let
    lref = LocalRef (Map.size m)
  in
    (lref, ActorMap (Map.insert lref (ActorData a s t) m))

data Action
  = SendAction LocalRef Message RemoteRef Promise
  | AsyncIOAction IOOp Promise
  | forall s. Typeable s => OnAction Promise (Resolution -> Free (ActorF s) ()) LocalRef
  | SetTimerAction Time.NominalDiffTime Promise
  | ClientResponseAction ClientRef Message  -- XXX: this doesn't really fit into action...

-- XXX: what about exceptions? transactional in state, but also in actions?!
actorMapTurn :: LocalRef -> Message -> Promise -> UTCTime -> Seed -> Log -> ActorMap
             -> ((Message, Promise, Seed, Log, ActorMap, [Action]), ActorMap)
actorMapTurn lref msg p t seed l am =
  case actorMapUnsafeLookup lref am of
    ActorData a _ _ ->
      (actorMapTurn' p [] lref t seed l (unActor (a msg)) am, am)

actorMapTurn' :: Promise -> [Action] -> LocalRef -> UTCTime -> Seed -> Log -> Free (ActorF s) a
              -> ActorMap -> (a, Promise, Seed, Log, ActorMap, [Action])
actorMapTurn' p acc _lref _t seed l (Pure msg) am = (msg, p, seed, l, am, reverse acc)
actorMapTurn' p acc  lref  t seed l (Free op)  am = case op of
  Invoke lref' msg k ->
    case actorMapUnsafeLookup lref' am of
      ActorData a' _ _ ->
        let (reply, p', seed', l', am', acc') =
              actorMapTurn' p acc lref' t seed l (unActor (a' msg)) am
        in
          actorMapTurn' p' acc' lref t seed' l' (k reply) am'
  Send rref msg k ->
    actorMapTurn' (p + 1) (SendAction lref msg rref p : acc) lref t seed l (k p) am
  AsyncIO io k ->
    actorMapTurn' (p + 1) (AsyncIOAction io p : acc) lref t seed l (k p) am
  On q c k ->
    actorMapTurn' p (OnAction q c lref : acc) lref t seed l (k ()) am
  Get k -> let s = fst (actorMapGetState lref am) in
             actorMapTurn' p acc lref t seed l (k s) am
  Put s' k -> actorMapTurn' p acc lref t seed l (k ()) (actorMapUpdateState lref s' am)
  GetTime k -> do
    actorMapTurn' p acc lref t seed l (k t) am
  Random k ->
    let
      (d, seed') = interval seed
    in
      actorMapTurn' p acc lref t seed' l (k d) am
  SetTimer ndt k ->
    actorMapTurn' (p + 1) (SetTimerAction ndt p : acc) lref t seed l (k p) am
  ClientResponse cref msg k ->
    actorMapTurn' p (ClientResponseAction cref msg : acc) lref t seed l (k ()) am
  DumpLog k ->
    actorMapTurn' p acc lref t seed l (k l) am


actorMapGetState :: Typeable s => LocalRef -> ActorMap -> (s, ActorMap)
actorMapGetState lref am = case actorMapUnsafeLookup lref am of
  ActorData _a s' _t -> case cast s' of
    Just s -> (s, am)
    Nothing -> error "actorMapGetState: impossible, cast failed"

------------------------------------------------------------------------

makeActorMapIO :: IO (TVar ActorMap)
makeActorMapIO = newTVarIO emptyActorMap

actorMapSpawnIO :: Typeable s => (Message -> Actor s) -> s -> Time -> TVar ActorMap
                -> IO LocalRef
actorMapSpawnIO a s t am = atomically (stateTVar am (actorMapSpawn a s t))

actorPokeIO :: EventLoop -> LocalRef -> Message -> IO Message
actorPokeIO ls lref msg = do
  now <- getCurrentTime (lsTime ls)
  (reply, as) <- atomically $ do
    p <- readTVar (lsNextPromise ls)
    seed <- readTVar (lsSeed ls)
    l <- readTVar (lsLog ls)
    (reply, as, p', seed', l') <- actorMapPokeSTM lref msg p now seed l (lsActorMap ls)
    writeTVar (lsNextPromise ls) p'
    writeTVar (lsSeed ls) seed'
    writeTVar (lsLog ls) l'
    return (reply, as)
  act ls as
  return reply
  where
    actorMapPokeSTM :: LocalRef -> Message -> Promise -> UTCTime -> Seed -> Log -> TVar ActorMap
                    -> STM (Message, [Action], Promise, Seed, Log)
    actorMapPokeSTM lref msg p t seed l am = stateTVar am (actorMapPoke lref msg p t seed l)
      where
        actorMapPoke :: LocalRef -> Message -> Promise -> UTCTime -> Seed -> Log -> ActorMap
                     -> ((Message, [Action], Promise, Seed, Log), ActorMap)
        actorMapPoke lref msg p t seed l am =
          let
            ((reply, p', seed', l', am', as), _am) = actorMapTurn lref msg p t seed l am
          in
            ((reply, as, p', seed', l'), am')

logEvent :: EventLoop -> LogEntry -> IO ()
logEvent ls e = atomically (modifyTVar (lsLog ls) (\(Log es) -> Log (e : es)))

logDump :: EventLoop -> IO String
logDump ls = do
  l <- readTVarIO (lsLog ls)
  return (getLog l)

logReset :: EventLoop -> IO ()
logReset ls = atomically (modifyTVar (lsLog ls) (const emptyLog))

------------------------------------------------------------------------

ainvoke :: EventLoop -> LocalRef -> Message -> IO Message
ainvoke ls lref msg = do
  returnVar <- newEmptyTMVarIO
  depth <- atomically (lengthTBQueue (lsQueue ls))
  reportEventLoopDepth depth (lsMetrics ls)
  atomically (writeTBQueue (lsQueue ls) (Admin (AdminInvoke lref msg returnVar)))
  atomically (takeTMVar returnVar)

asend :: EventLoop -> RemoteRef -> Message -> IO (Async Message)
asend ls rref msg = do
  p <- atomically (stateTVar (lsNextPromise ls) (\p -> (p, p + 1)))
  returnVar <- newEmptyTMVarIO
  atomically (writeTBQueue (lsQueue ls) (Admin (AdminSend rref msg p returnVar)))
  async (atomically (takeTMVar returnVar))

clientRequest :: EventLoop -> LocalRef -> Message -> IO (Message, Async Message)
clientRequest ls lref msg = do
  cref <- atomically (stateTVar (lsNextPromise ls) (\p -> (ClientRef (unPromise p), p + 1)))
  returnVar <- newEmptyTMVarIO
  respVar <- newEmptyTMVarIO
  atomically (modifyTVar' (lsAsyncState ls)
              (\s -> s { asyncStateClientResponses =
                         Map.insert cref respVar (asyncStateClientResponses s) }))
  depth <- atomically (lengthTBQueue (lsQueue ls))
  reportEventLoopDepth depth (lsMetrics ls)
  atomically (writeTBQueue (lsQueue ls) (ClientRequestEvent lref msg cref returnVar))
  a <- async (atomically (takeTMVar respVar))
  reply <- atomically (takeTMVar returnVar)
  return (reply, a)

spawn :: Typeable s => EventLoop -> (Message -> Actor s) -> s -> IO LocalRef
spawn ls a s = do
  returnVar <- newEmptyTMVarIO
  atomically (writeTBQueue (lsQueue ls) (Admin (Spawn a s returnVar)))
  atomically (takeTMVar returnVar)

quit :: EventLoop -> IO ()
quit ls = atomically (writeTBQueue (lsQueue ls) (Admin Quit))

------------------------------------------------------------------------

data ResolutionClosure =
  forall s. Typeable s => ResolutionClosure (Resolution -> Free (ActorF s) ())

data AsyncState = AsyncState
  { asyncStateAsyncIO         :: Map (Async IOResult) Promise
  , asyncStateContinuations   :: Map Promise (ResolutionClosure, LocalRef)
  , asyncStateAdminSend       :: Map Promise (TMVar Message)
  , asyncStateTimeouts        :: Heap (Entry UTCTime (TimeoutKind, Promise))
  , asyncStateClientResponses :: Map ClientRef (TMVar Message)
  }

data TimeoutKind
  = SendTimeout
  | TimerTimeout
  deriving Show

emptyAsyncState :: AsyncState
emptyAsyncState = AsyncState Map.empty Map.empty Map.empty Heap.empty Map.empty

madePromises :: [Action] -> Set Int
madePromises = foldMap go
  where
    go (SendAction _from _msg _to (Promise i)) = Set.singleton i
    go (AsyncIOAction _io (Promise i)) = Set.singleton i
    go OnAction {} = Set.empty
    go (SetTimerAction _ndt (Promise i)) = Set.singleton i
    go ClientResponseAction {} = Set.empty

act :: EventLoop -> [Action] -> IO ()
act ls as = mapM_ go as
  where
    is :: Set Int
    is = madePromises as

    go :: Action -> IO ()
    go (SendAction from msg to p@(Promise i)) = do
      -- XXX: What do we do if `transportSend` fails here? We should probably
      -- call the failure handler/continuation for this promise, if it exists.
      -- If it doesn't exist we probably want to crash the sender, i.e. `from`.
      transportSend (lsTransport ls)
        (Envelope RequestKind (localToRemoteRef (lsName ls) from) msg to (CorrelationId i))
      t <- timestamp (lsLogicalTime ls)
      logEvent ls (LogSend from msg to t)
      t <- getCurrentTime (lsTime ls)
      -- XXX: make it possible to specify when a send request should timeout.
      let timeoutAfter = Time.addUTCTime 60 t
      atomically $ modifyTVar' (lsAsyncState ls) $
        \s -> s { asyncStateTimeouts =
                    Heap.insert (Entry timeoutAfter (SendTimeout, p))
                                (asyncStateTimeouts s) }
    go (AsyncIOAction io p) = do
      -- XXX: Append to lsIOQueue if thread pool is activated...
      a <- async (diskIO io (lsDisk ls))
      -- XXX: make it possible for async I/O to timeout as well?
      atomically $ modifyTVar' (lsAsyncState ls) $ \s ->
        s { asyncStateAsyncIO = Map.insert a p (asyncStateAsyncIO s) }
    go (OnAction p@(Promise i) k lref)
      | i `Set.member` is =
          atomically $ modifyTVar' (lsAsyncState ls) $ \s ->
            s { asyncStateContinuations =
                Map.insert p (ResolutionClosure k, lref) (asyncStateContinuations s) }
      | otherwise =
          error "act: impossible, `On` must be supplied with a promise that was just made."
    go (SetTimerAction ndt p) = do
      t <- getCurrentTime (lsTime ls)
      let timeoutAfter = Time.addUTCTime ndt t
      atomically $ modifyTVar' (lsAsyncState ls) $ \s ->
        s { asyncStateTimeouts =
            Heap.insert (Entry timeoutAfter (TimerTimeout, p)) (asyncStateTimeouts s) }
    go (ClientResponseAction cref msg) = atomically $ do
      s <- readTVar (lsAsyncState ls)
      let respVar = asyncStateClientResponses s Map.! cref -- XXX: partial
      putTMVar respVar msg
      modifyTVar' (lsAsyncState ls) $ \s ->
        s { asyncStateClientResponses = Map.delete cref (asyncStateClientResponses s) }

data Reaction
  = Receive Promise Envelope
  | forall s. Typeable s => SendTimeoutReaction (Free (ActorF s) ()) LocalRef
  | AsyncIOFinished Promise IOResult
  | AsyncIOFailed Promise SomeException

data ReactTask
  = NothingToDo
  | Request Envelope
  | forall s. Typeable s => ResumeContinuation (Free (ActorF s) ()) LocalRef
  | AdminSendResponse (TMVar Message) Message

react :: Reaction -> AsyncState -> (ReactTask, AsyncState)
react (Receive p e) s =
  case envelopeKind e of
    RequestKind  -> (Request e, s)
    ResponseKind ->
      case Map.lookup p (asyncStateContinuations s) of
        Just (ResolutionClosure k, lref) ->
          (ResumeContinuation (k (InternalMessageR (envelopeMessage e))) lref,
            s { asyncStateContinuations =
                  Map.delete p (asyncStateContinuations s) })
        Nothing ->
          case Map.lookup p (asyncStateAdminSend s) of
            Nothing ->
              -- We got a response for something we are not (longer) waiting for.
              (NothingToDo, s)
            Just returnVar ->
              (AdminSendResponse returnVar (envelopeMessage e),
                s { asyncStateAdminSend =
                      Map.delete p (asyncStateAdminSend s) })
react (SendTimeoutReaction a lref) s = (ResumeContinuation a lref, s)
react (AsyncIOFinished p result) s =
  case Map.lookup p (asyncStateContinuations s) of
    Nothing ->
      -- No continuation was registered for this async.
      (NothingToDo, s)
    Just (ResolutionClosure k, lref) ->
      (ResumeContinuation (k (IOResultR result)) lref,
        s { asyncStateContinuations =
              Map.delete p (asyncStateContinuations s) })
react (AsyncIOFailed p exception) s =
  case Map.lookup p (asyncStateContinuations s) of
    Nothing ->
      -- No continuation was registered for this async.
      (NothingToDo, s)
    Just (ResolutionClosure k, lref) ->
      (ResumeContinuation (k (ExceptionR exception)) lref,
        s { asyncStateContinuations =
              Map.delete p (asyncStateContinuations s) })

reactIO :: Reaction -> TVar AsyncState -> IO ReactTask
reactIO r v = atomically (stateTVar v (react r))

------------------------------------------------------------------------

data Event
  = Action Action
  | Reaction Reaction
  | Admin Command
  | AdminCommands [AdminCommand]
  | ClientRequestEvent LocalRef Message ClientRef (TMVar Message)

data Command
  = forall s. Typeable s => Spawn (Message -> Actor s) s (TMVar LocalRef)
  | AdminInvoke LocalRef Message (TMVar Message)
  | AdminSend RemoteRef Message Promise (TMVar Message)
  -- XXX: DumpLog (TMVar [LogEntry])
  | Quit

data EventLoop = EventLoop
  { lsName           :: EventLoopName
  , lsActorMap       :: TVar ActorMap
  , lsAsyncState     :: TVar AsyncState
  , lsQueue          :: TBQueue Event
  , lsTime           :: Time -- Physical time.
  , lsLogicalTime    :: LogicalTime
  , lsSeed           :: TVar Seed
  , lsTransport      :: Transport IO
  , lsAdminTransport :: AdminTransport
  , lsDisk           :: Disk IO
  , lsPids           :: TVar [Async ()]
  , lsNextPromise    :: TVar Promise
  , lsLog            :: TVar Log
  , lsMetrics        :: Metrics
  , lsIOQueue        :: TBQueue (IOOp, Promise)
  }

initLoopState :: EventLoopName -> Time -> Seed -> Transport IO -> AdminTransport -> Disk IO
              -> IO EventLoop
initLoopState name time seed transport adminTransport disk =
  EventLoop
    <$> pure name
    <*> newTVarIO emptyActorMap
    <*> newTVarIO emptyAsyncState
    <*> newTBQueueIO 128
    <*> pure time
    <*> newLogicalTime (fromString (getEventLoopName name))
    <*> newTVarIO seed
    <*> pure transport
    <*> pure adminTransport
    <*> pure disk
    <*> newTVarIO []
    <*> newTVarIO (Promise 0)
    <*> newTVarIO emptyLog
    <*> newMetrics
    <*> newTBQueueIO 128 -- XXX: longer?

isDoneEventLoop :: EventLoop -> STM Bool
isDoneEventLoop ls =
  (&&)
  <$> isEmptyTBQueue (lsQueue ls)
  <*> fmap isDoneAsyncState (readTVar (lsAsyncState ls))

isDoneAsyncState :: AsyncState -> Bool
isDoneAsyncState as = and
  [ Map.null  (asyncStateAsyncIO as)
  , Map.null  (asyncStateContinuations as)
  , Map.null  (asyncStateAdminSend as)
  , Heap.null (asyncStateTimeouts as)
  , Map.null  (asyncStateClientResponses as)
  ]

isDoneEventLoopModuloTimeouts :: EventLoop -> STM Bool
isDoneEventLoopModuloTimeouts ls =
  (&&)
  <$> isEmptyTBQueue (lsQueue ls)
  <*> fmap isDoneAsyncStateModuloTimeouts (readTVar (lsAsyncState ls))

isDoneAsyncStateModuloTimeouts :: AsyncState -> Bool
isDoneAsyncStateModuloTimeouts as = and
  [ Map.null (asyncStateAsyncIO as)
  , Map.null (asyncStateContinuations as)
  , Map.null (asyncStateAdminSend as)
  , not (Heap.null (asyncStateTimeouts as))
  , Map.null (asyncStateClientResponses as)
  ]

waitForEventLoop :: EventLoop -> IO ()
waitForEventLoop ls = atomically (check =<< isDoneEventLoop ls)

waitForEventLoopModuloTimeouts :: EventLoop -> IO ()
waitForEventLoopModuloTimeouts ls =
  atomically (check =<< isDoneEventLoopModuloTimeouts ls)

dumpEventLoop :: EventLoop -> IO ()
dumpEventLoop ls = do
  as <- readTVarIO (lsAsyncState ls)
  dumpAsyncState as

dumpAsyncState :: AsyncState -> IO ()
dumpAsyncState as = do
  putStrLn ""
  putStrLn "    AsyncState"
  putStr "      { asyncStateAsyncIO = "
  print (Map.elems (asyncStateAsyncIO as))
  putStr "      , asyncStateContinuations = "
  print (map (fmap snd) (Map.toList (asyncStateContinuations as)))
  putStr "      , asyncStateAdminSend = "
  print (Map.keys (asyncStateAdminSend as))
  putStr "      , asyncStateTimeouts = "
  print (toList (asyncStateTimeouts as))
  putStr "      , asyncStateClientResponses = "
  print (Map.keys (asyncStateClientResponses as))
  putStrLn "    }"


withTransport :: TransportKind -> Codec -> EventLoopName -> (Transport IO -> IO a) -> IO a
withTransport tk codec name k =
  bracket
    (case tk of
       NamedPipe fp -> namedPipeTransport fp name
       Http port    -> httpTransport port
       HttpSync     -> httpSyncTransport codec
       Stm          -> stmTransport)
    transportShutdown
    k

data Threaded = SingleThreaded | MultiThreaded

data ThreadPool = NoThreadPool | ThreadPoolOfSize Int

asyncIOWorker :: EventLoop -> IO ()
asyncIOWorker ls = do
  (io, p) <- atomically (readTBQueue (lsIOQueue ls))
  -- XXX: timeout?
  eResult <- try (diskIO io (lsDisk ls))
  case eResult of
    Right result ->
      atomically (writeTBQueue (lsQueue ls) (Reaction (AsyncIOFinished p result)))
    Left e ->
      atomically (writeTBQueue (lsQueue ls) (Reaction (AsyncIOFailed p e)))

spawnIOWorkers :: ThreadPool -> EventLoop -> IO [Async ()]
spawnIOWorkers NoThreadPool         _ls = return []
spawnIOWorkers (ThreadPoolOfSize n)  ls
  | n <= 0 = error "spawnIOWorkers: thread pool less than or equal to zero"
  | otherwise = do
      tid <- myThreadId
      (cap, _pinned) <- threadCapability tid
      numCap <- getNumCapabilities
      -- Minus one because we don't want to run the workers on the same thread
      -- as the main event loop.
      when (n > numCap - 1) $
        error "spawnIOWorkers: thread pool size is bigger than the available CPU capabilities"
      mapM (\c -> asyncOn c (asyncIOWorker ls)) [ i | i <- [0..n], i /= cap ]

makeEventLoop :: Time -> Seed -> TransportKind -> AdminTransportKind -> Codec -> DiskKind
              -> EventLoopName -> IO EventLoop
makeEventLoop = makeEventLoopThreaded SingleThreaded NoThreadPool

makeEventLoopThreaded :: Threaded -> ThreadPool -> Time -> Seed -> TransportKind
                      -> AdminTransportKind -> Codec -> DiskKind -> EventLoopName
                      -> IO EventLoop
makeEventLoopThreaded threaded threadpool time seed tk atk codec dk name = do
  t <- case tk of
         NamedPipe fp -> namedPipeTransport fp name
         Http port    -> httpTransport port
         HttpSync     -> httpSyncTransport codec
         Stm          -> stmTransport
  at <- case atk of
         AdminNamedPipe fp -> namedPipeAdminTransport fp name
  d <- case dk of
         FakeDisk    -> fakeDisk
         RealDisk fp -> realDisk fp
  ls <- initLoopState name time seed t at d
  workerPids <- spawnIOWorkers threadpool ls
  pids <- case threaded of
            SingleThreaded ->
              fmap (: [])
                (async (runHandlers seed  -- XXX: or do we want `TVar Seed` here?
                        [ handleInbound1 ls
                        , handleAdminCommands1 ls
                        , handleAsyncIO1 ls
                        , handleEvents1 ls
                        , handleTimeouts1 ls
                        ]))
            MultiThreaded ->
              mapM async [ handleInbound ls
                         , handleAdminCommands ls
                         , handleAsyncIO ls
                         , handleEvents ls
                         , handleTimeouts ls
                         ]
  atomically (modifyTVar' (lsPids ls) ((workerPids ++ pids) ++))
  mapM_ link pids
  return ls

  {-
withEventLoop :: EventLoopName -> Codec -> (EventLoop -> FakeTimeHandle -> IO a) -> IO a
withEventLoop name codec k =
  withTransport (NamedPipe "/tmp") codec name $ \t -> do
    (time, h) <- fakeTimeEpoch
    let seed = makeSeed 0
    disk <- fakeDisk
    ls <- initLoopState name time seed t disk
    a <- async (runHandlers seed
                 [ handleInbound1 ls
                 , handleAsyncIO1 ls
                 , handleEvents1 ls
                 , handleTimeouts1 ls
                 ])
    atomically (modifyTVar' (lsPids ls) (a :))
    x <- k ls h
    quit ls
    return x
-}

runHandlers :: Seed -> [IO ()] -> IO ()
runHandlers seed0 hs = go seed0
  where
    hss :: Vector [IO ()]
    hss = Vector.fromList (permutations hs)

    go :: Seed -> IO ()
    go seed =
      let
        (ix, seed') = randomR (0, length hss - 1) seed
      in do
        sequence_ (hss Vector.! ix)
        threadDelay 100
        go seed'

handleInbound :: EventLoop -> IO ()
handleInbound = forever . handleInbound1

handleInbound1 :: EventLoop -> IO ()
handleInbound1 ls = do
  -- XXX: instead of just reading one message from the transport queue we could
  -- read the whole queue here... See `adminTransportReceive`.
  me <- transportReceive (lsTransport ls)
  case me of
    Nothing -> return ()
    Just e  -> do
      let p = Promise (getCorrelationId (envelopeCorrelationId e))
      atomically (writeTBQueue (lsQueue ls) (Reaction (Receive p e)))

handleAdminCommands :: EventLoop -> IO ()
handleAdminCommands = forever . handleAdminCommands1

handleAdminCommands1 :: EventLoop -> IO ()
handleAdminCommands1 ls = do
  threadDelay 10000
  cmds <- adminTransportReceive (lsAdminTransport ls)
  case cmds of
    []    -> return ()
    _ : _ -> atomically (writeTBQueue (lsQueue ls) (AdminCommands cmds))

handleAsyncIO :: EventLoop -> IO ()
handleAsyncIO ls = forever (handleAsyncIO1 ls >> threadDelay 1000 {- 1 ms -})

handleAsyncIO1 :: EventLoop -> IO ()
handleAsyncIO1 ls = atomically $ do
  s <- readTVar (lsAsyncState ls)
  -- We want to be non-blocking here, otherwise we can get into a situation
  -- where we schedule a slow I/O operation and block waiting for it while
  -- other quicker I/O operation could get scheduled, but won't be polled
  -- until after the slow one finishes.
  mr <- pollAnySTM (Map.keys (asyncStateAsyncIO s))
  case mr of
    Nothing -> return ()
    Just (a, Right ioResult) -> do
      let p = asyncStateAsyncIO s Map.! a -- XXX: partial function
      writeTBQueue (lsQueue ls) (Reaction (AsyncIOFinished p ioResult))
      writeTVar (lsAsyncState ls)
        (s { asyncStateAsyncIO = Map.delete a (asyncStateAsyncIO s) })
    Just (a, Left exception) -> do
      let p = asyncStateAsyncIO s Map.! a -- XXX: partial function
      writeTBQueue (lsQueue ls) (Reaction (AsyncIOFailed p exception))
      writeTVar (lsAsyncState ls)
        (s { asyncStateAsyncIO = Map.delete a (asyncStateAsyncIO s) })

-- | Check if any async finished in a non-blocking way.
pollAnySTM :: [Async a] -> STM (Maybe (Async a, Either SomeException a))
pollAnySTM []       = return Nothing
pollAnySTM (a : as) = do
  mr <- pollSTM a
  case mr of
    Nothing -> pollAnySTM as
    Just r  -> return (Just (a, r))

handleTimeouts :: EventLoop -> IO ()
handleTimeouts = forever . handleTimeouts1

handleTimeouts1 :: EventLoop -> IO ()
handleTimeouts1 ls = do
  now <- getCurrentTime (lsTime ls)
  als <- atomically (stateTVar (lsAsyncState ls) (findTimedout now))
  mapM_ (\(ExistsStateActor a, lref) ->
           atomically (writeTBQueue (lsQueue ls) (Reaction (SendTimeoutReaction a lref))))
    als

data ExistsStateActor =
  forall s. Typeable s => ExistsStateActor (Free (ActorF s) ())

findTimedout :: UTCTime -> AsyncState
             -> ([(ExistsStateActor, LocalRef)], AsyncState)
findTimedout now s =
  let
    (timedout, heap') = Heap.span (\(Entry t _p) -> t <= now) (asyncStateTimeouts s)
    ts = map Heap.payload (toList timedout)
    cs = catMaybes (map (\(tk, p) ->
                           fmap (\c -> (tk, c)) (Map.lookup p (asyncStateContinuations s))) ts)
    als = map ((\(tk, (ResolutionClosure c, lref)) -> case tk of
                   SendTimeout  -> (ExistsStateActor (c TimeoutR), lref)
                   TimerTimeout -> (ExistsStateActor (c TimerR), lref))) cs
    ks = foldr Map.delete (asyncStateContinuations s) (map snd ts)
  in
    (als, s { asyncStateContinuations = ks
            , asyncStateTimeouts      = heap'
            })

handleEvents :: EventLoop -> IO ()
handleEvents = forever . handleEvents1Blocking

handleEvents1Blocking :: EventLoop -> IO ()
handleEvents1Blocking ls = do
  e <- atomically (readTBQueue (lsQueue ls))
  handleEvent e ls
    `catch` \(ex :: SomeException) -> do
        reportError (lsMetrics ls)
        -- XXX: Why are `AsyncCancelled` being caught here?
        unless (fromException ex == Just AsyncCancelled) $
          putStrLn ("handleEvents: exception: " ++ show ex)

-- XXX: Using this non-blocking version in `handleEvents` causes tests to be a
-- lot more flaky, not sure why.
handleEvents1 :: EventLoop -> IO ()
handleEvents1 ls = do
  me <- atomically (tryReadTBQueue (lsQueue ls))
  case me of
    Nothing -> return ()
    Just e  -> do
      handleEvent e ls
        `catch` \(ex :: SomeException) -> do
                  -- XXX: Why are `AsyncCancelled` being caught here?
                  if fromException ex == Just AsyncCancelled
                  then exitSuccess
                  else putStrLn ("handleEvents: exception: " ++ show ex)

handleEvent :: Event -> EventLoop -> IO ()
handleEvent (Action a)   ls = act ls [a]
handleEvent (Reaction r) ls = do
  m <- reactIO r (lsAsyncState ls)
  case m of
    NothingToDo -> return ()
    Request e -> do
      let lref = remoteToLocalRef (envelopeReceiver e)
      reply <- actorPokeIO ls lref (envelopeMessage e)
      -- XXX: return more than reply, and log event
      transportSend (lsTransport ls) (replyEnvelope e reply)
    ResumeContinuation a lref -> do
      now <- getCurrentTime (lsTime ls)
      as <- atomically $ do
        am   <- readTVar (lsActorMap ls)
        p    <- readTVar (lsNextPromise ls)
        seed <- readTVar (lsSeed ls)
        l    <- readTVar (lsLog ls)
        let ((), p', seed', l', am', as) = actorMapTurn' p [] lref now seed l a am
        writeTVar (lsActorMap ls) am'
        writeTVar (lsNextPromise ls) p'
        writeTVar (lsSeed ls) seed'
        writeTVar (lsLog ls) l'
        return as
      act ls as
    AdminSendResponse returnVar msg ->
      atomically (putTMVar returnVar msg)
handleEvent (Admin cmd) ls = case cmd of
  Spawn a s returnVar -> do
    lref <- actorMapSpawnIO a s (lsTime ls) (lsActorMap ls)
    atomically (putTMVar returnVar lref)
  AdminInvoke lref msg returnVar -> do
    reply <- actorPokeIO ls lref msg
    -- XXX: return more than reply, and log event
    atomically (putTMVar returnVar reply)
  AdminSend rref msg p returnVar -> do
    let dummyAdminRef = localToRemoteRef (lsName ls) (LocalRef (-1))
    transportSend (lsTransport ls)
      (Envelope RequestKind dummyAdminRef msg rref (CorrelationId (unPromise p)))
    atomically (modifyTVar' (lsAsyncState ls)
                (\as -> as { asyncStateAdminSend =
                             Map.insert p returnVar (asyncStateAdminSend as) }))
  Quit -> do
    pids <- readTVarIO (lsPids ls)
    threadDelay 100000
    mapM_ cancel pids
handleEvent (AdminCommands cmds) ls = mapM_ go cmds
  where
    go :: AdminCommand -> IO ()
    -- XXX: we probably don't want two different ways to quit, or more generally
    -- issue admin commands. The biggest problem is spawning, since actors are
    -- not seralisable... Perhaps we could give actors names and "load" all
    -- available actors at the start of the event loop, then the spawn admin
    -- command can simply take the name of the actors and start it? Or skip
    -- spawning completely and simply specify the actors you want to spawn upon
    -- event loop start up? Once we got supervisors, we probably want to specify
    -- the root supervisor at event loop start up and have it do the
    -- (re)spawning, so perhaps removing the spawn admin command completely is
    -- the way to go...
    go AdminQuit     = do
      putStrLn "Shutting down..."
      pids <- readTVarIO (lsPids ls)
      threadDelay 100000
      adminTransportShutdown (lsAdminTransport ls)
      mapM_ uninterruptibleCancel pids
      -- XXX: ^ The above isn't enough to actually shutdown the event loop. We
      -- need to catch the AsyncCancelled exception in `handleEvents1` and do an
      -- `exitSuccess` there... Perhaps a more clean way would be to have a
      -- shutdown `TMVar` which `runHandlers` checks before looping?
    go AdminDumpLog  = do
      putStrLn "dumping log"
      s <- logDump ls
      adminTransportSend (lsAdminTransport ls) s
    go AdminResetLog = do
      putStrLn "resetting log"
      logReset ls

handleEvent (ClientRequestEvent lref msg cref returnVar) ls = do
  reply <- actorPokeIO ls lref (ClientRequest' (getMessage msg) (getArgs msg) cref)
  -- XXX: return more than reply, so we can log event
  atomically (putTMVar returnVar reply)

waitForEventLoopQuit :: EventLoop -> IO ()
waitForEventLoopQuit ls = do
  pids <- readTVarIO (lsPids ls)
  mapM_ wait pids
