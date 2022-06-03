{-# LANGUAGE ExistentialQuantification #-}
module Lec05.ClientGenerator where

import Control.Monad (forM_)
import Data.ByteString.Lazy (ByteString)
import Data.IORef
import qualified Data.Map as Map

import Lec05.Event
import Lec05.StateMachine
import Lec05.Time

data NextClientRequest
  = CurrentlyNoRequests
  | Now Event
  | Later Time (IO Event)

data ClientGenerator = ClientGenerator
  { cgRespond :: ClientId -> ByteString -> IO ()
  , cgNextClientRequest :: IO NextClientRequest
  }

emptyGenerator :: ClientGenerator
emptyGenerator = ClientGenerator
  { cgRespond = \ _ _ -> return ()
  , cgNextClientRequest = return CurrentlyNoRequests
  }

data SingleStateGenerator = forall s. SingleStateGenerator
  { ssgInit :: s
  , ssgNext :: s -> s
  , ssgGen  :: ClientId -> s -> (NodeId, ByteString)
  }

data SingleStateGeneratorState s
  = SGSActive s
  | SGSWaiting Time s

singleStateGenerator :: SingleStateGenerator -> Clock -> NominalDiffTime
  -> ClientId -> IO ClientGenerator
singleStateGenerator (SingleStateGenerator initgs next gen) clock delay clientId = do
  startingTime <- cGetCurrentTime clock
  ref <- newIORef (SGSWaiting startingTime initgs)
  return $ ClientGenerator
    { cgRespond = \_ _ -> do
        st <- readIORef ref
        case st of
          SGSActive gs -> do
            now <- cGetCurrentTime clock
            let nextTime = addTimeSeconds delay now
            writeIORef ref (SGSWaiting nextTime (next gs))
          SGSWaiting{} -> error "singleStateGenerator got response when not expected"
    , cgNextClientRequest = do
        st <- readIORef ref
        case st of
          SGSActive{} -> return CurrentlyNoRequests
          SGSWaiting t gs -> do
            let
              (node, msg) = gen clientId gs
              ev = NetworkEventE (NetworkEvent node (ClientRequest t clientId msg))
              action = do
                writeIORef ref (SGSActive gs)
                return ev
            return (Later t action)
    }

runManyIndependent :: (ClientId -> IO ClientGenerator) -> [ClientId] -> IO ClientGenerator
runManyIndependent _constructor [] = return emptyGenerator
runManyIndependent  constructor cs = do
  ref <- newIORef mempty
  forM_ cs $ \c -> do
    g <- constructor c
    modifyIORef' ref $ Map.insert c g
  return $ ClientGenerator
    { cgRespond = \ c b -> do
        gens <- readIORef ref
        case Map.lookup c gens of
          Nothing -> error "Client response from unknown client"
          Just g  -> do
            cgRespond g c b
    , cgNextClientRequest = do
        gens <- readIORef ref
        go (Map.elems gens) -- this is deterministic and fine
    }
  where
    go [] = return CurrentlyNoRequests
    go (g:gs) = do
      a <- cgNextClientRequest g
      case a of
        CurrentlyNoRequests -> go gs
        Now{} -> return a
        Later t e -> go' t e gs

    go' t e [] = return $ Later t e
    go' t e (g:gs) = do
      a <- cgNextClientRequest g
      case a of
        CurrentlyNoRequests -> go gs
        Now{} -> return a
        Later t' e'
          | t <= t'   -> go' t  e  gs
          | otherwise -> go' t' e' gs


data GeneratorSchema
  = NoGenerator
  | SingleState SingleStateGenerator NominalDiffTime
  | Multiple SingleStateGenerator NominalDiffTime Int

makeGenerator :: GeneratorSchema -> Clock -> IO ClientGenerator
makeGenerator NoGenerator _clock = return emptyGenerator
makeGenerator (SingleState ssg delay) clock = singleStateGenerator ssg clock delay (ClientId 0)
makeGenerator (Multiple ssg delay n) clock
  = runManyIndependent (singleStateGenerator ssg clock delay) [ClientId i | i <- [0..pred n]]
