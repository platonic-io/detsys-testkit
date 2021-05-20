{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE ExistentialQuantification #-}

module StuntDouble.ActorMap where

import Control.Concurrent.STM
import Data.Map (Map)
import qualified Data.Map as Map

import StuntDouble.Actor (IOResult)
import StuntDouble.Actor.State
import StuntDouble.FreeMonad
import StuntDouble.Message
import StuntDouble.Reference

------------------------------------------------------------------------

newtype Promise a = Promise Int
  deriving Num

newtype Actor = Actor { unActor :: Free ActorF Message }

data ActorF x
  = Invoke LocalRef Message (Message -> x)
  | Send RemoteRef Message (Promise Message -> x)
  | AsyncIO (IO IOResult) (Promise IOResult -> x)
  | forall a. On (Promise a) (a -> x) (() -> x)
  | Get (State -> x)
  | Put State (() -> x)
deriving instance Functor ActorF

------------------------------------------------------------------------

newtype ActorMap = ActorMap (Map LocalRef (Message -> Actor, State))

actorMapLookup :: LocalRef -> ActorMap -> Maybe (Message -> Actor, State)
actorMapLookup lref (ActorMap m) = Map.lookup lref m

actorMapUnsafeLookup :: LocalRef -> ActorMap -> (Message -> Actor, State)
actorMapUnsafeLookup lref am = case actorMapLookup lref am of
  Nothing -> error ("actorMapUnsafeLookup: `" ++ show lref ++ "' not in actor map.")
  Just v  -> v

emptyActorMap :: ActorMap
emptyActorMap = ActorMap Map.empty

actorMapSpawn :: (Message -> Actor) -> State -> ActorMap -> (LocalRef, ActorMap)
actorMapSpawn a s (ActorMap m) =
  let
    lref = LocalRef (Map.size m)
  in
    (lref, ActorMap (Map.insert lref (a, s) m))

data Action
  = SendAction RemoteRef Message (Promise Message)
  | AsyncIOAction (IO IOResult) (Promise IOResult)
  | forall a. OnAction (Promise a) (a -> Actor) LocalRef

-- XXX: what about exceptions? transactional in state, but also in actions?!
actorMapTurn :: LocalRef -> Message -> ActorMap -> ((Message, ActorMap, [Action]), ActorMap)
actorMapTurn lref0 msg0 am0 =
  let
    a = fst (actorMapUnsafeLookup lref0 am0)
  in
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
          go (pc + 1) (SendAction rref msg p : acc) lref (k p) am
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
