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
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set

import StuntDouble.Actor (IOResult)
import StuntDouble.Actor.State
import StuntDouble.EventLoop.Event
       ( CorrelationId(..)
       , Envelope(..)
       , EnvelopeKind(..)
       , getCorrelationId
       , replyEnvelope
       )
import StuntDouble.EventLoop.Transport
import StuntDouble.EventLoop.Transport.Http
import StuntDouble.FreeMonad
import StuntDouble.Message
import StuntDouble.Reference

------------------------------------------------------------------------

newtype Promise = Promise Int
  deriving (Eq, Ord, Num)

unPromise :: Promise -> Int
unPromise (Promise i) = i

newtype Actor = Actor { unActor :: Free ActorF Message }

type Resolution = Either SomeException (Either IOResult Message)

data ActorF x
  = Invoke LocalRef Message (Message -> x)
  | Send RemoteRef Message (Promise -> x)
  | AsyncIO (IO IOResult) (Promise -> x)
  | On Promise (Resolution -> Free ActorF ()) (() -> x)
  | Get (State -> x)
  | Put State (() -> x)
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

------------------------------------------------------------------------

newtype ActorMap = ActorMap (Map LocalRef (Message -> Actor, State))

emptyActorMap :: ActorMap
emptyActorMap = ActorMap Map.empty

actorMapLookup :: LocalRef -> ActorMap -> Maybe (Message -> Actor, State)
actorMapLookup lref (ActorMap m) = Map.lookup lref m

actorMapUnsafeLookup :: LocalRef -> ActorMap -> (Message -> Actor, State)
actorMapUnsafeLookup lref am = case actorMapLookup lref am of
  Nothing -> error ("actorMapUnsafeLookup: `" ++ show lref ++ "' not in actor map.")
  Just v  -> v

actorMapSpawn :: (Message -> Actor) -> State -> ActorMap -> (LocalRef, ActorMap)
actorMapSpawn a s (ActorMap m) =
  let
    lref = LocalRef (Map.size m)
  in
    (lref, ActorMap (Map.insert lref (a, s) m))

data Action
  = SendAction LocalRef Message RemoteRef Promise
  | AsyncIOAction (IO IOResult) Promise
  | OnAction Promise (Resolution -> Free ActorF ()) LocalRef

-- XXX: what about exceptions? transactional in state, but also in actions?!
actorMapTurn :: LocalRef -> Message -> ActorMap
             -> ((Message, Promise, ActorMap, [Action]), ActorMap)
actorMapTurn lref0 msg0 am0 =
  let
    a = fst (actorMapUnsafeLookup lref0 am0)
  in
    -- XXX: Promises should not always start from 0, or they will overlap each
    -- other if more than one turn happens...
    (actorMapTurn' (Promise 0) [] lref0 (unActor (a msg0)) am0, am0)

actorMapTurn' :: Promise -> [Action] -> LocalRef -> Free ActorF a -> ActorMap
              -> (a, Promise, ActorMap, [Action])
actorMapTurn' p acc _lref (Pure msg) am = (msg, p, am, reverse acc)
actorMapTurn' p acc  lref (Free op)  am = case op of
  Invoke lref' msg k ->
    let
      a' = fst (actorMapUnsafeLookup lref' am)
      (reply, p', am', acc') = actorMapTurn' p acc lref' (unActor (a' msg)) am
    in
      actorMapTurn' p' acc' lref (k reply) am'
  Send rref msg k ->
    actorMapTurn' (p + 1) (SendAction lref msg rref p : acc) lref (k p) am
  AsyncIO io k ->
    actorMapTurn' (p + 1) (AsyncIOAction io p : acc) lref (k p) am
  On q c k ->
    actorMapTurn' p (OnAction q c lref : acc) lref (k ()) am
  Get k ->
    actorMapTurn' p acc lref (k (snd (actorMapUnsafeLookup lref am))) am
  Put s' k ->
    case am of
      ActorMap m ->
        actorMapTurn' p acc lref (k ()) (ActorMap (Map.adjust (\(a, _s) -> (a, s')) lref m))

actorMapPeek :: LocalRef -> Message -> ActorMap -> (Message, ActorMap)
actorMapPeek lref msg am =
  let
    ((reply, _p, _am', _as), _am) = actorMapTurn lref msg am
  in
    (reply, am)

actorMapPoke :: LocalRef -> Message -> ActorMap -> ((Message, [Action]), ActorMap)
actorMapPoke lref msg am =
  let
    ((reply, _p, am', as), _am) = actorMapTurn lref msg am
  in
    ((reply, as), am')

actorMapGetState :: LocalRef -> ActorMap -> (State, ActorMap)
actorMapGetState lref am = (snd (actorMapUnsafeLookup lref am), am)

------------------------------------------------------------------------

makeActorMapIO :: IO (TVar ActorMap)
makeActorMapIO = newTVarIO emptyActorMap

actorMapSpawnIO :: (Message -> Actor) -> State -> TVar ActorMap -> IO LocalRef
actorMapSpawnIO a s am = atomically (stateTVar am (actorMapSpawn a s))

actorMapTurnIO :: LocalRef -> Message -> TVar ActorMap
               -> IO (Message, Promise, ActorMap, [Action])
actorMapTurnIO lref msg am = atomically (stateTVar am (actorMapTurn lref msg))

actorMapPeekIO :: LocalRef -> Message -> TVar ActorMap -> IO Message
actorMapPeekIO lref msg am = atomically (stateTVar am (actorMapPeek lref msg))

actorMapPokeIO :: LocalRef -> Message -> TVar ActorMap -> IO (Message, [Action])
actorMapPokeIO lref msg am = atomically (stateTVar am (actorMapPoke lref msg))

actorPokeIO :: EventLoop -> LocalRef -> Message -> IO Message
actorPokeIO ls lref msg = do
  (reply, as) <- actorMapPokeIO lref msg (lsActorMap ls)
  act' ls as
  return reply

act' :: EventLoop -> [Action] -> IO ()
act' ls as = do
  -- XXX: non-atomic update of the async state?!
  s <- readTVarIO (lsAsyncState ls)
  s' <- act (lsName ls) as (lsTransport ls) s
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

spawn :: EventLoop -> (Message -> Actor) -> State -> IO LocalRef
spawn ls a s = do
  returnVar <- newEmptyTMVarIO
  atomically (writeTBQueue (lsQueue ls) (Admin (Spawn a s returnVar)))
  atomically (takeTMVar returnVar)

quit :: EventLoop -> IO ()
quit ls = atomically (writeTBQueue (lsQueue ls) (Admin Quit))

------------------------------------------------------------------------

data AsyncState = AsyncState
  { asyncStateAsyncIO       :: Map (Async IOResult) Promise
  , asyncStateContinuations :: Map Promise (Resolution -> Free ActorF (), LocalRef)
  , asyncStateAdminSend     :: Map Promise (TMVar Message)
  }

emptyAsyncState :: AsyncState
emptyAsyncState = AsyncState Map.empty Map.empty Map.empty

madePromises :: [Action] -> Set Int
madePromises = foldMap go
  where
    go (SendAction _from _msg _to (Promise i)) = Set.singleton i
    go (AsyncIOAction _io (Promise i)) = Set.singleton i
    go OnAction {} = Set.empty

act :: EventLoopName -> [Action] -> Transport IO -> AsyncState -> IO AsyncState
act name as t s0 = foldM go s0 as
  where
    is :: Set Int
    is = madePromises as

    go :: AsyncState -> Action -> IO AsyncState
    go s (SendAction from msg to (Promise i)) = do
      transportSend t
        (Envelope RequestKind (localToRemoteRef name from) msg to (CorrelationId i))
      -- XXX: make a note of when we sent so we can timeout.
      return s
    go s (AsyncIOAction io p) = do
      -- XXX: Use `asyncOn` a different capability than main loop.
      a <- async io
      return (s { asyncStateAsyncIO = Map.insert a p (asyncStateAsyncIO s) })
    go s (OnAction p@(Promise i) k lref)
      | i `Set.member` is =
          return (s { asyncStateContinuations =
                      Map.insert p (k, lref) (asyncStateContinuations s) })
      | otherwise =
          error "act: impossible, `On` must be supplied with a promise that was just made."

data Reaction
  = Receive Promise Envelope
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
        Just (k, lref) ->  (ResumeContinuation (k (Right (Right (envelopeMessage e)))) lref,
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
react (AsyncIOFinished p result) s =
  case Map.lookup p (asyncStateContinuations s) of
    Nothing ->
      -- No continuation was registered for this async.
      (NothingToDo, s)
    Just (k, lref) -> (ResumeContinuation (k (Right (Left result))) lref,
                        s { asyncStateContinuations =
                            Map.delete p (asyncStateContinuations s) })
react (AsyncIOFailed p exception) s =
  case Map.lookup p (asyncStateContinuations s) of
    Nothing ->
      -- No continuation was registered for this async.
      (NothingToDo, s)
    Just (k, lref) -> (ResumeContinuation (k (Left exception)) lref,
                        s { asyncStateContinuations =
                            Map.delete p (asyncStateContinuations s) })

reactIO :: Reaction -> TVar AsyncState -> IO ReactTask
reactIO r v = atomically (stateTVar v (react r))

------------------------------------------------------------------------

data Event
  = Action Action
  | Reaction Reaction
  | Admin Command

data Command
  = Spawn (Message -> Actor) State (TMVar LocalRef)
  | AdminInvoke LocalRef Message (TMVar Message)
  | AdminSend RemoteRef Message Promise (TMVar Message)
  | Quit

data EventLoop = EventLoop
  { lsName        :: EventLoopName
  , lsActorMap    :: TVar ActorMap
  , lsAsyncState  :: TVar AsyncState
  , lsQueue       :: TBQueue Event
  , lsTransport   :: Transport IO
  , lsPids        :: TVar [Async ()]
  , lsNextPromise :: TVar Promise
  , lsLog         :: TVar [LogEntry]
  }

data LogEntry
  = LogEntry

initLoopState :: EventLoopName -> Transport IO -> IO EventLoop
initLoopState name t =
  EventLoop
    <$> pure name
    <*> newTVarIO emptyActorMap
    <*> newTVarIO emptyAsyncState
    <*> newTBQueueIO 128
    <*> pure t
    <*> newTVarIO []
    <*> newTVarIO (Promise 0)
    <*> newTVarIO []

makeEventLoop :: TransportKind -> EventLoopName -> IO EventLoop
makeEventLoop tk name = do
  t <- case tk of
         NamedPipe fp -> namedPipeTransport fp name
         Http port    -> httpTransport port
  ls <- initLoopState name t
  aInHandler <- async (handleInbound ls)
  aAsyncIOHandler <- async (handleAsyncIO ls)
  aEvHandler <- async (handleEvents ls)
  let pids = [aInHandler, aEvHandler, aAsyncIOHandler]
  atomically (modifyTVar' (lsPids ls) (pids ++))
  mapM_ link pids
  return ls

handleInbound :: EventLoop -> IO ()
handleInbound ls = forever go
  where
    go = do
      e <- transportReceive (lsTransport ls)
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

handleEvents :: EventLoop -> IO ()
handleEvents ls = forever go
  where
    go = do
      e <- atomically (readTBQueue (lsQueue ls))
      handleEvent e ls
        `catch` \(ex :: SomeException) ->
                  putStrLn ("handleEvents: exception: " ++ show ex)

handleEvent :: Event -> EventLoop -> IO ()
handleEvent (Action a) ls = act' ls [a]
handleEvent (Reaction r) ls = do
  m <- reactIO r (lsAsyncState ls)
  case m of
    NothingToDo -> return ()
    Request e -> do
      let lref = remoteToLocalRef (envelopeReceiver e)
      reply <- actorPokeIO ls lref (envelopeMessage e)
      transportSend (lsTransport ls) (replyEnvelope e reply)
    ResumeContinuation a lref -> do
      as <- atomically $ do
        am <- readTVar (lsActorMap ls)
        p  <- readTVar (lsNextPromise ls)
        let ((), p', am', as) = actorMapTurn' p [] lref a am
        writeTVar (lsActorMap ls) am'
        writeTVar (lsNextPromise ls) p'
        return as
      act' ls as
    AdminSendResponse returnVar msg ->
      atomically (putTMVar returnVar msg)
handleEvent (Admin cmd) ls = case cmd of
  Spawn a s returnVar -> do
    lref <- actorMapSpawnIO a s (lsActorMap ls)
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
