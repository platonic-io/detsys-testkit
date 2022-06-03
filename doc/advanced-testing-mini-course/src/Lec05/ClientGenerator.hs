{-# LANGUAGE ExistentialQuantification #-}
module Lec05.ClientGenerator where

import Data.ByteString.Lazy (ByteString)
import Data.IORef

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
  , ssgGen  :: s -> (NodeId, ByteString)
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
              (node, msg) = gen gs
              ev = NetworkEventE (NetworkEvent node (ClientRequest t clientId msg))
              action = do
                writeIORef ref (SGSActive gs)
                return ev
            return (Later t action)
    }

data GeneratorSchema
  = NoGenerator
  | SingleState SingleStateGenerator NominalDiffTime

makeGenerator :: GeneratorSchema -> Clock -> IO ClientGenerator
makeGenerator NoGenerator _clock = return emptyGenerator
makeGenerator (SingleState ssg delay) clock = singleStateGenerator ssg clock delay (ClientId 0)
