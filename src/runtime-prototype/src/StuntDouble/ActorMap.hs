{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}

module StuntDouble.ActorMap where

import Control.Concurrent
import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Exception
import Control.Monad
import Data.Foldable (toList)
import Data.Maybe (catMaybes)
import Data.Heap (Heap, Entry(Entry))
import qualified Data.Heap as Heap
import Data.Time (UTCTime)
import qualified Data.Time as Time
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set

import StuntDouble.Actor.State
import StuntDouble.Envelope
import StuntDouble.Transport
import StuntDouble.Transport.NamedPipe
import StuntDouble.Transport.Http
import StuntDouble.Transport.Stm
import StuntDouble.FreeMonad
import StuntDouble.Time
import StuntDouble.Log
import StuntDouble.Random
import StuntDouble.Message
import StuntDouble.Reference

------------------------------------------------------------------------

data IOResult = String String | IOUnit ()

newtype Promise = Promise Int
  deriving (Eq, Ord, Num)

unPromise :: Promise -> Int
unPromise (Promise i) = i

newtype Actor = Actor { unActor :: Free ActorF Message }

-- XXX: Should we always force the user to supply an exception continuation?
data Resolution
  = TimeoutR
  | TimerR
  | IOResultR IOResult
  | InternalMessageR Message
  | ExceptionR SomeException

data ActorF x
  = Invoke LocalRef Message (Message -> x)
  | Send RemoteRef Message (Promise -> x)
  | AsyncIO (IO IOResult) (Promise -> x)
  | On Promise (Resolution -> Free ActorF ()) (() -> x)
  | Get (State -> x)
  | Put State (() -> x)
  | GetTime (UTCTime -> x)
  | Random (Double -> x)
  | SetTimer Time.NominalDiffTime (Promise -> x)
  | ClientResponse ClientRef Message (() -> x)
  -- XXX: Log?
  -- XXX: Throw?
deriving instance Functor ActorF

invoke :: LocalRef -> Message -> Free ActorF Message
invoke lref msg = Free (Invoke lref msg return)

send :: RemoteRef -> Message -> Free ActorF Promise
send rref msg = Free (Send rref msg return)

asyncIO :: IO IOResult -> Free ActorF Promise
asyncIO io = Free (AsyncIO io return)

on :: Promise -> (Resolution -> Free ActorF ()) -> Free ActorF ()
on p k = Free (On p k return)

get :: Free ActorF State
get = Free (Get return)

put :: State -> Free ActorF ()
put s' = Free (Put s' return)

modify :: (State -> State) -> Free ActorF ()
modify f = put . f =<< get

getTime :: Free ActorF UTCTime
getTime = Free (GetTime return)

random :: Free ActorF Double
random = Free (Random return)

setTimer :: Time.NominalDiffTime -> Free ActorF Promise
setTimer ndt = Free (SetTimer ndt return)

clientResponse :: ClientRef -> Message -> Free ActorF ()
clientResponse cref msg = Free (ClientResponse cref msg return)

------------------------------------------------------------------------

newtype ActorMap = ActorMap (Map LocalRef ActorData)

data ActorData = ActorData
  { adActor :: Message -> Actor
  , adState :: State
  , adTime  :: Time
  }

emptyActorMap :: ActorMap
emptyActorMap = ActorMap Map.empty

actorMapLookup :: LocalRef -> ActorMap -> Maybe ActorData
actorMapLookup lref (ActorMap m) = Map.lookup lref m

actorMapUnsafeLookup :: LocalRef -> ActorMap -> ActorData
actorMapUnsafeLookup lref am = case actorMapLookup lref am of
  Nothing -> error ("actorMapUnsafeLookup: `" ++ show lref ++ "' not in actor map.")
  Just v  -> v

actorMapSpawn :: (Message -> Actor) -> State -> Time -> ActorMap -> (LocalRef, ActorMap)
actorMapSpawn a s t (ActorMap m) =
  let
    lref = LocalRef (Map.size m)
  in
    (lref, ActorMap (Map.insert lref (ActorData a s t) m))

data Action
  = SendAction LocalRef Message RemoteRef Promise
  | AsyncIOAction (IO IOResult) Promise
  | OnAction Promise (Resolution -> Free ActorF ()) LocalRef
  | SetTimerAction Time.NominalDiffTime Promise
  | ClientResponseAction ClientRef Message  -- XXX: this doesn't really fit into action...

-- XXX: what about exceptions? transactional in state, but also in actions?!
actorMapTurn :: LocalRef -> Message -> Promise -> UTCTime -> Seed -> ActorMap
             -> ((Message, Promise, Seed, ActorMap, [Action]), ActorMap)
actorMapTurn lref msg p t seed am =
  let
    a = adActor (actorMapUnsafeLookup lref am)
  in
    (actorMapTurn' p [] lref t seed (unActor (a msg)) am, am)

actorMapTurn' :: Promise -> [Action] -> LocalRef -> UTCTime -> Seed -> Free ActorF a
              -> ActorMap -> (a, Promise, Seed, ActorMap, [Action])
actorMapTurn' p acc _lref _t seed (Pure msg) am = (msg, p, seed, am, reverse acc)
actorMapTurn' p acc  lref  t seed (Free op)  am = case op of
  Invoke lref' msg k ->
    let
      a' = adActor (actorMapUnsafeLookup lref' am)
      (reply, p', seed', am', acc') = actorMapTurn' p acc lref' t seed (unActor (a' msg)) am
    in
      actorMapTurn' p' acc' lref t seed' (k reply) am'
  Send rref msg k ->
    actorMapTurn' (p + 1) (SendAction lref msg rref p : acc) lref t seed (k p) am
  AsyncIO io k ->
    actorMapTurn' (p + 1) (AsyncIOAction io p : acc) lref t seed (k p) am
  On q c k ->
    actorMapTurn' p (OnAction q c lref : acc) lref t seed (k ()) am
  Get k ->
    actorMapTurn' p acc lref t seed (k (adState (actorMapUnsafeLookup lref am))) am
  Put s' k ->
    case am of
      ActorMap m ->
        actorMapTurn' p acc lref t seed (k ())
        (ActorMap (Map.adjust (\(ActorData a _s t') -> ActorData a s' t') lref m))
  GetTime k -> do
    actorMapTurn' p acc lref t seed (k t) am
  Random k ->
    let
      (d, seed') = uniform seed
    in
      actorMapTurn' p acc lref t seed' (k d) am
  SetTimer ndt k ->
    actorMapTurn' (p + 1) (SetTimerAction ndt p : acc) lref t seed (k p) am
  ClientResponse cref msg k ->
    actorMapTurn' p (ClientResponseAction cref msg : acc) lref t seed (k ()) am

actorMapPeek :: LocalRef -> Message -> Promise -> UTCTime -> Seed -> ActorMap
             -> (Message, ActorMap)
actorMapPeek lref msg p t seed am =
  let
    ((reply, _p, _seed, _am', _as), _am) = actorMapTurn lref msg p t seed am
  in
    (reply, am)

actorMapPoke :: LocalRef -> Message -> Promise -> UTCTime -> Seed -> ActorMap
             -> ((Message, [Action], Promise, Seed), ActorMap)
actorMapPoke lref msg p t seed am =
  let
    ((reply, p', seed', am', as), _am) = actorMapTurn lref msg p t seed am
  in
    ((reply, as, p', seed'), am')

actorMapGetState :: LocalRef -> ActorMap -> (State, ActorMap)
actorMapGetState lref am = (adState (actorMapUnsafeLookup lref am), am)

------------------------------------------------------------------------

makeActorMapIO :: IO (TVar ActorMap)
makeActorMapIO = newTVarIO emptyActorMap

actorMapSpawnIO :: (Message -> Actor) -> State -> Time -> TVar ActorMap -> IO LocalRef
actorMapSpawnIO a s t am = atomically (stateTVar am (actorMapSpawn a s t))

actorMapTurnIO :: LocalRef -> Message -> Promise -> UTCTime -> Seed -> TVar ActorMap
               -> IO (Message, Promise, Seed, ActorMap, [Action])
actorMapTurnIO lref msg p t seed am =
  atomically (stateTVar am (actorMapTurn lref msg p t seed))

actorMapPeekIO :: LocalRef -> Message -> Promise -> UTCTime -> Seed -> TVar ActorMap
               -> IO Message
actorMapPeekIO lref msg p t seed am =
  atomically (stateTVar am (actorMapPeek lref msg p t seed))

actorMapPokeSTM :: LocalRef -> Message -> Promise -> UTCTime -> Seed -> TVar ActorMap
                -> STM (Message, [Action], Promise, Seed)
actorMapPokeSTM lref msg p t seed am = stateTVar am (actorMapPoke lref msg p t seed)

actorMapPokeIO :: LocalRef -> Message -> Promise -> UTCTime -> Seed -> TVar ActorMap
               -> IO (Message, [Action], Promise, Seed)
actorMapPokeIO lref msg p t seed am = atomically (actorMapPokeSTM lref msg p t seed am)

actorPokeIO :: EventLoop -> LocalRef -> Message -> IO Message
actorPokeIO ls lref msg = do
  now <- getCurrentTime (lsTime ls)
  (reply, as) <- atomically $ do
    p <- readTVar (lsNextPromise ls)
    seed <- readTVar (lsSeed ls)
    (reply, as, p', seed') <- actorMapPokeSTM lref msg p now seed (lsActorMap ls)
    writeTVar (lsNextPromise ls) p'
    writeTVar (lsSeed ls) seed'
    return (reply, as)
  act' ls as
  return reply

act' :: EventLoop -> [Action] -> IO ()
act' ls as = do
  -- XXX: non-atomic update of the async state?!
  s <- readTVarIO (lsAsyncState ls)
  s' <- act (lsName ls) as (lsTime ls) (lsTransport ls) s
  atomically (writeTVar (lsAsyncState ls) s')

actorMapGetStateIO :: LocalRef -> TVar ActorMap -> IO State
actorMapGetStateIO lref am = atomically (stateTVar am (actorMapGetState lref))

getActorState :: EventLoop -> LocalRef -> IO State
getActorState ls lref = actorMapGetStateIO lref (lsActorMap ls)

------------------------------------------------------------------------

ainvoke :: EventLoop -> LocalRef -> Message -> IO Message
ainvoke ls lref msg = do
  returnVar <- newEmptyTMVarIO
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
  atomically (writeTBQueue (lsQueue ls) (ClientRequestEvent lref msg cref returnVar))
  a <- async (atomically (takeTMVar respVar))
  reply <- atomically (takeTMVar returnVar)
  return (reply, a)

spawn :: EventLoop -> (Message -> Actor) -> State -> IO LocalRef
spawn ls a s = do
  returnVar <- newEmptyTMVarIO
  atomically (writeTBQueue (lsQueue ls) (Admin (Spawn a s returnVar)))
  atomically (takeTMVar returnVar)

quit :: EventLoop -> IO ()
quit ls = atomically (writeTBQueue (lsQueue ls) (Admin Quit))

------------------------------------------------------------------------

data AsyncState = AsyncState
  { asyncStateAsyncIO         :: Map (Async IOResult) Promise
  , asyncStateContinuations   :: Map Promise (Resolution -> Free ActorF (), LocalRef)
  , asyncStateAdminSend       :: Map Promise (TMVar Message)
  , asyncStateTimeouts        :: Heap (Entry UTCTime (TimeoutKind, Promise))
  , asyncStateClientResponses :: Map ClientRef (TMVar Message)
  }

data TimeoutKind
  = SendTimeout
  | TimerTimeout

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

act :: EventLoopName -> [Action] -> Time -> Transport IO -> AsyncState -> IO AsyncState
act name as time transport s0 = foldM go s0 as
  where
    is :: Set Int
    is = madePromises as

    go :: AsyncState -> Action -> IO AsyncState
    go s (SendAction from msg to p@(Promise i)) = do
      transportSend transport
        (Envelope RequestKind (localToRemoteRef name from) msg to (CorrelationId i))
      t <- getCurrentTime time
      -- XXX: make it possible to specify when a send request should timeout.
      let timeoutAfter = Time.addUTCTime 60 t
      return s { asyncStateTimeouts =
                   Heap.insert (Entry timeoutAfter (SendTimeout, p)) (asyncStateTimeouts s) }
    go s (AsyncIOAction io p) = do
      -- XXX: Use `asyncOn` a different capability than main loop.
      a <- async io
      -- XXX: make it possible for async I/O to timeout as well?
      return (s { asyncStateAsyncIO = Map.insert a p (asyncStateAsyncIO s) })
    go s (OnAction p@(Promise i) k lref)
      | i `Set.member` is =
          return (s { asyncStateContinuations =
                      Map.insert p (k, lref) (asyncStateContinuations s) })
      | otherwise =
          error "act: impossible, `On` must be supplied with a promise that was just made."
    go s (SetTimerAction ndt p) = do
      t <- getCurrentTime time
      let timeoutAfter = Time.addUTCTime ndt t
      return s { asyncStateTimeouts =
                   Heap.insert (Entry timeoutAfter (TimerTimeout, p)) (asyncStateTimeouts s) }
    go s (ClientResponseAction cref msg) = do
      let respVar = asyncStateClientResponses s Map.! cref -- XXX: partial
      atomically (putTMVar respVar msg)
      return s
        { asyncStateClientResponses = Map.delete cref (asyncStateClientResponses s) }

data Reaction
  = Receive Promise Envelope
  | SendTimeoutReaction (Free ActorF ()) LocalRef
  | AsyncIOFinished Promise IOResult
  | AsyncIOFailed Promise SomeException

data ReactTask
  = NothingToDo
  | Request Envelope
  | ResumeContinuation (Free ActorF ()) LocalRef
  | AdminSendResponse (TMVar Message) Message

react :: Reaction -> AsyncState -> (ReactTask, AsyncState)
react (Receive p e) s =
  case envelopeKind e of
    RequestKind  -> (Request e, s)
    ResponseKind ->
      case Map.lookup p (asyncStateContinuations s) of
        Just (k, lref) -> (ResumeContinuation (k (InternalMessageR (envelopeMessage e))) lref,
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
    Just (k, lref) -> (ResumeContinuation (k (IOResultR result)) lref,
                        s { asyncStateContinuations =
                            Map.delete p (asyncStateContinuations s) })
react (AsyncIOFailed p exception) s =
  case Map.lookup p (asyncStateContinuations s) of
    Nothing ->
      -- No continuation was registered for this async.
      (NothingToDo, s)
    Just (k, lref) -> (ResumeContinuation (k (ExceptionR exception)) lref,
                        s { asyncStateContinuations =
                            Map.delete p (asyncStateContinuations s) })

reactIO :: Reaction -> TVar AsyncState -> IO ReactTask
reactIO r v = atomically (stateTVar v (react r))

------------------------------------------------------------------------

data Event
  = Action Action
  | Reaction Reaction
  | Admin Command
  | ClientRequestEvent LocalRef Message ClientRef (TMVar Message)

data Command
  = Spawn (Message -> Actor) State (TMVar LocalRef)
  | AdminInvoke LocalRef Message (TMVar Message)
  | AdminSend RemoteRef Message Promise (TMVar Message)
  -- XXX: DumpLog (TMVar [LogEntry])
  | Quit

data EventLoop = EventLoop
  { lsName        :: EventLoopName
  , lsActorMap    :: TVar ActorMap
  , lsAsyncState  :: TVar AsyncState
  , lsQueue       :: TBQueue Event
  , lsTime        :: Time
  , lsSeed        :: TVar Seed
  , lsTransport   :: Transport IO
  , lsPids        :: TVar [Async ()]
  , lsNextPromise :: TVar Promise
  , lsLog         :: TVar Log
  }

initLoopState :: EventLoopName -> Time -> Seed -> Transport IO -> IO EventLoop
initLoopState name time seed t =
  EventLoop
    <$> pure name
    <*> newTVarIO emptyActorMap
    <*> newTVarIO emptyAsyncState
    <*> newTBQueueIO 128
    <*> pure time
    <*> newTVarIO seed
    <*> pure t
    <*> newTVarIO []
    <*> newTVarIO (Promise 0)
    <*> newTVarIO emptyLog

makeEventLoop :: Time -> Seed -> TransportKind -> EventLoopName -> IO EventLoop
makeEventLoop time seed tk name = do
  t <- case tk of
         NamedPipe fp -> namedPipeTransport fp name
         Http port    -> httpTransport port
         Stm          -> stmTransport
  ls <- initLoopState name time seed t
  -- XXX: all these async handlers introduce non-determinism, we would need a
  -- way to synchronise them if we wanted complete determinism...
  aInHandler <- async (handleInbound ls)
  aAsyncIOHandler <- async (handleAsyncIO ls)
  aEvHandler <- async (handleEvents ls)
  aTimeoutHandler <- async (handleTimeouts ls)
  let pids = [aInHandler, aAsyncIOHandler, aEvHandler, aTimeoutHandler]
  atomically (modifyTVar' (lsPids ls) (pids ++))
  mapM_ link pids
  return ls

handleInbound :: EventLoop -> IO ()
handleInbound ls = forever go
  where
    go = do
      me <- transportReceive (lsTransport ls)
      case me of
        Nothing -> return ()
        Just e  -> do
          let p = Promise (getCorrelationId (envelopeCorrelationId e))
          atomically (writeTBQueue (lsQueue ls) (Reaction (Receive p e)))

handleAsyncIO :: EventLoop -> IO ()
handleAsyncIO ls = forever (go >> threadDelay 1000 {- 1 ms -})
  where
    go :: IO ()
    go = atomically $ do
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
handleTimeouts ls = forever go
  where
    go :: IO ()
    go = do
      now <- getCurrentTime (lsTime ls)
      als <- atomically (stateTVar (lsAsyncState ls) (findTimedout now))
      mapM_ (\(a, lref) ->
               atomically (writeTBQueue (lsQueue ls) (Reaction (SendTimeoutReaction a lref))))
        als

findTimedout :: UTCTime -> AsyncState
             -> ([(Free ActorF (), LocalRef)], AsyncState)
findTimedout now s =
  let
    (timedout, heap') = Heap.span (\(Entry t _p) -> t <= now) (asyncStateTimeouts s)
    ts = map Heap.payload (toList timedout)
    cs = catMaybes (map (\(tk, p) ->
                           fmap (\c -> (tk, c)) (Map.lookup p (asyncStateContinuations s))) ts)
    als = map ((\(tk, (c, lref)) -> case tk of
                   SendTimeout  -> (c TimeoutR, lref)
                   TimerTimeout -> (c TimerR, lref))) cs
    ks = foldr Map.delete (asyncStateContinuations s) (map snd ts)
  in
    (als, s { asyncStateContinuations = ks
            , asyncStateTimeouts      = heap'
            })

handleEvents :: EventLoop -> IO ()
handleEvents ls = forever go
  where
    go = do
      e <- atomically (readTBQueue (lsQueue ls))
      handleEvent e ls
        `catch` \(ex :: SomeException) ->
                  putStrLn ("handleEvents: exception: " ++ show ex)

handleEvent :: Event -> EventLoop -> IO ()
handleEvent (Action a)   ls = act' ls [a]
handleEvent (Reaction r) ls = do
  m <- reactIO r (lsAsyncState ls)
  case m of
    NothingToDo -> return ()
    Request e -> do
      let lref = remoteToLocalRef (envelopeReceiver e)
      reply <- actorPokeIO ls lref (envelopeMessage e)
      transportSend (lsTransport ls) (replyEnvelope e reply)
    ResumeContinuation a lref -> do
      now <- getCurrentTime (lsTime ls)
      as <- atomically $ do
        am   <- readTVar (lsActorMap ls)
        p    <- readTVar (lsNextPromise ls)
        seed <- readTVar (lsSeed ls)
        let ((), p', seed', am', as) = actorMapTurn' p [] lref now seed a am
        writeTVar (lsActorMap ls) am'
        writeTVar (lsNextPromise ls) p'
        writeTVar (lsSeed ls) seed'
        return as
      act' ls as
    AdminSendResponse returnVar msg ->
      atomically (putTMVar returnVar msg)
handleEvent (Admin cmd) ls = case cmd of
  Spawn a s returnVar -> do
    lref <- actorMapSpawnIO a s (lsTime ls) (lsActorMap ls)
    atomically (putTMVar returnVar lref)
  AdminInvoke lref msg returnVar -> do
    reply <- actorPokeIO ls lref msg
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
handleEvent (ClientRequestEvent lref msg cref returnVar) ls = do
  reply <- actorPokeIO ls lref (ClientRequest (getMessage msg) cref)
  atomically (putTMVar returnVar reply)
