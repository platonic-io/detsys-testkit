{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}

module StuntDouble.ActorMap where

import Control.Exception
import Control.Concurrent.Async
import Control.Concurrent.STM
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
       )
import StuntDouble.EventLoop.Transport
import StuntDouble.EventLoop.Transport.Http
import StuntDouble.FreeMonad
import StuntDouble.Message
import StuntDouble.Reference

------------------------------------------------------------------------

newtype Promise = Promise Int
  deriving (Eq, Ord, Num)

newtype Actor = Actor { unActor :: Free ActorF Message }

data ActorF x
  = Invoke LocalRef Message (Message -> x)
  | Send RemoteRef Message (Promise -> x)
  | AsyncIO (IO IOResult) (Promise -> x)
  | On Promise (Either IOResult Message -> x) (() -> x)
  | Get (State -> x)
  | Put State (() -> x)
deriving instance Functor ActorF

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
  | OnAction Promise (Either IOResult Message -> Actor) LocalRef

-- XXX: what about exceptions? transactional in state, but also in actions?!
actorMapTurn :: LocalRef -> Message -> ActorMap -> ((Message, ActorMap, [Action]), ActorMap)
actorMapTurn lref0 msg0 am0 =
  let
    a = fst (actorMapUnsafeLookup lref0 am0)
  in
    -- XXX: Promises should not always start from 0, or they will overlap each
    -- other if more than one turn happens...
    (go 0 [] lref0 (unActor (a msg0)) am0, am0)
  where
    go _pc acc _lref (Pure msg) am = (msg, am, reverse acc)
    go  pc acc  lref (Free op)  am = case op of
      Invoke lref' msg k ->
        let
          a' = fst (actorMapUnsafeLookup lref' am)
          (reply, am', acc') = go pc acc lref' (unActor (a' msg)) am
        in
          go pc acc' lref (k reply) am'
      Send rref msg k ->
        let
          p = Promise pc
        in
          go (pc + 1) (SendAction lref msg rref p : acc) lref (k p) am
      AsyncIO io k ->
        let
          p = Promise pc
        in
          go (pc + 1) (AsyncIOAction io p : acc) lref (k p) am
      On p c k ->
        go pc (OnAction p (Actor . c) lref : acc) lref (k ()) am
      Get k ->
        go pc acc lref (k (snd (actorMapUnsafeLookup lref am))) am
      Put s' k ->
        case am of
          ActorMap m ->
            go pc acc lref (k ()) (ActorMap (Map.adjust (\(a, _s) -> (a, s')) lref m))

actorMapPeek :: LocalRef -> Message -> ActorMap -> (Message, ActorMap)
actorMapPeek lref msg am =
  let
    ((reply, _am', _as), _am) = actorMapTurn lref msg am
  in
    (reply, am)

actorMapPoke :: LocalRef -> Message -> ActorMap -> (Message, ActorMap)
actorMapPoke lref msg am =
  let
    ((reply, am', _as), _am) = actorMapTurn lref msg am
  in
    (reply, am')

------------------------------------------------------------------------

type ActorMapTVar = TVar ActorMap

makeActorMapIO :: IO ActorMapTVar
makeActorMapIO = newTVarIO emptyActorMap

actorMapSpawnIO :: (Message -> Actor) -> State -> ActorMapTVar -> IO LocalRef
actorMapSpawnIO a s am = atomically (stateTVar am (actorMapSpawn a s))

actorMapTurnIO :: LocalRef -> Message -> ActorMapTVar -> IO (Message, ActorMap, [Action])
actorMapTurnIO lref msg am = atomically (stateTVar am (actorMapTurn lref msg))

actorMapPeekIO :: LocalRef -> Message -> ActorMapTVar -> IO Message
actorMapPeekIO lref msg am = atomically (stateTVar am (actorMapPeek lref msg))

actorMapPokeIO :: LocalRef -> Message -> ActorMapTVar -> IO Message
actorMapPokeIO lref msg am = atomically (stateTVar am (actorMapPoke lref msg))

------------------------------------------------------------------------

devSend :: {- EventLoopRef-} RemoteRef -> Message -> IO (Async Message)
devSend = undefined
  -- p <- createPromise
  -- v <- newEmptyTMVar
  -- insertDeveloperSend p v
  -- async (atomically (takeTMVar v))

------------------------------------------------------------------------

data AsyncState = AsyncState
  { asyncStateAsyncIO         :: Map (Async IOResult) Promise
  , asyncStateContinuations   :: Map Promise (Either IOResult Message -> Actor, LocalRef)
  -- , asyncStateDeveloperSend   :: Map Promise (TMVar Message)
  }

emptyAsyncState :: AsyncState
emptyAsyncState = AsyncState Map.empty Map.empty

madePromises :: [Action] -> Set Int
madePromises = foldMap go
  where
    go (SendAction _from _msg _to (Promise i)) = Set.singleton i
    go (AsyncIOAction _io (Promise i)) = Set.singleton i
    go OnAction {} = Set.empty

act :: EventLoopName -> [Action] -> AsyncState -> Transport IO -> IO AsyncState
act name as s0 t = foldM go s0 as
  where
    is :: Set Int
    is = madePromises as

    go :: AsyncState -> Action -> IO AsyncState
    go s (SendAction from msg to (Promise i)) = do
      transportSend t
        (Envelope RequestKind (localToRemoteRef name from) msg to (CorrelationId i))
      return s -- XXX: make a note of when we sent so we can timeout.
    go s (AsyncIOAction io p) = do
      a <- async io -- XXX: Use `asyncOn` a different capability than main loop.
      return (s { asyncStateAsyncIO = Map.insert a p (asyncStateAsyncIO s) })
    go s (OnAction p@(Promise i) k lref)
      | i `Set.member` is = do
          return (s { asyncStateContinuations =
                      Map.insert p (k, lref) (asyncStateContinuations s) })
      | otherwise =
          error "act: impossible, `On` must be supplied with a promise that was just made."

data Reaction
  = Response Promise Message
  | AsyncIOFinished (Async IOResult) IOResult

react :: Reaction -> AsyncState -> (Maybe (Actor, LocalRef), AsyncState)
react (Response p msg) s =
  case Map.lookup p (asyncStateContinuations s) of
    Just (k, lref) ->  (Just (k (Right msg), lref),
                        s { asyncStateContinuations =
                              Map.delete p (asyncStateContinuations s) })
    Nothing ->
      -- XXX: Map.lookup p (developerSend s)?

      -- We got a response for something we are not (longer) waiting for.
      (Nothing, s)
react (AsyncIOFinished a result) s =
  case Map.lookup a (asyncStateAsyncIO s) of
    Nothing -> error "react: impossible, unknown async finished."
    Just p -> case Map.lookup p (asyncStateContinuations s) of
      Nothing ->
        -- No continuation was registered for this async.
        -- XXX: the async handler should take care for this map deletion...
        (Nothing, s { asyncStateAsyncIO = Map.delete a (asyncStateAsyncIO s) })
      Just (k, lref) -> (Just (k (Left result), lref),
                         s { asyncStateAsyncIO       = Map.delete a (asyncStateAsyncIO s)
                           , asyncStateContinuations = Map.delete p (asyncStateContinuations s)
                           })

reactIO :: Reaction -> TVar AsyncState -> IO (Maybe (Actor, LocalRef))
reactIO r v = atomically (stateTVar v (react r))

------------------------------------------------------------------------

data Event
  = Action Action
  | Reaction Reaction

data EventLoop = EventLoop
  { lsActorMap   :: TVar ActorMap
  , lsAsyncState :: TVar AsyncState
  , lsQueue      :: TBQueue Event
  , lsTransport  :: Transport IO
  , lsPids       :: TVar [Async ()]
  }

initLoopState :: Transport IO -> IO EventLoop
initLoopState t =
  EventLoop
    <$> newTVarIO emptyActorMap
    <*> newTVarIO emptyAsyncState
    <*> newTBQueueIO 128
    <*> pure t
    <*> newTVarIO []

makeEventLoop :: TransportKind -> EventLoopName -> IO EventLoop
makeEventLoop tk name = do
  t <- case tk of
         NamedPipe fp -> namedPipeTransport fp name
         Http port    -> httpTransport port
  ls <- initLoopState t
  aInHandler <- async (handleInbound ls)
  aAsyncIOHandler <- async (handleAsyncIO ls)
  aEvHandler <- async (handleEvents ls)
  atomically (modifyTVar' (lsPids ls)
               ([aInHandler, aEvHandler, aAsyncIOHandler] ++))
  return ls

handleInbound :: EventLoop -> IO ()
handleInbound ls = forever go
  where
    go = do
      e <- transportReceive (lsTransport ls)
      let p = Promise (getCorrelationId (envelopeCorrelationId e))
      atomically (writeTBQueue (lsQueue ls) (Reaction (Response p (envelopeMessage e))))

handleAsyncIO :: EventLoop -> IO ()
handleAsyncIO ls = forever go
  where
    go = atomically $ do
      -- XXX: Use waitAnyCatchSTM and handle exceptions appropriately here, e.g.
      -- by extending `AsyncIODone` with `Fail` and `Info`.
      as <- readTVar (lsAsyncState ls)
      (a, ioResult) <- waitAnySTM (Map.keys (asyncStateAsyncIO as))
      writeTBQueue (lsQueue ls) (Reaction (AsyncIOFinished a ioResult))
      writeTVar (lsAsyncState ls)
        (as { asyncStateAsyncIO = Map.delete a (asyncStateAsyncIO as) })

handleEvents :: EventLoop -> IO ()
handleEvents ls = forever go
  where
    go = do
      e <- atomically (readTBQueue (lsQueue ls))
      handleEvent e ls
        `catch` \(ex :: SomeException) ->
                  putStrLn ("handleEvents: exception: " ++ show ex)

handleEvent :: Event -> EventLoop -> IO ()
handleEvent (Action a) ls = undefined
-- XXX: act :: EventLoopName -> [Action] -> AsyncState -> Transport IO -> IO AsyncState
handleEvent (Reaction r) ls = do
  m <- reactIO r (lsAsyncState ls)
  case m of
    Nothing -> return ()
    Just (a, lref) -> undefined
    -- XXX: need a variant of actorMapTurn which doesn't take a message...
