{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}

module Lec05.History where

import Control.Concurrent.STM
import Data.Typeable
import Data.TreeDiff (ToExpr)

import Lec02ConcurrentSMTesting (History', Operation'(..), Pid(Pid))
import qualified Lec02ConcurrentSMTesting as Lec2
import Lec05.StateMachine

------------------------------------------------------------------------

newtype History = History (TQueue HistEvent)

data HistEvent = forall state req msg resp.
  ( Show state, Show req, Show msg, Show resp
  , ToExpr state
  , Typeable req, Typeable resp
  ) => HistEvent NodeId state (Input req msg) state [Output resp msg]
deriving instance Show HistEvent

newHistory :: IO History
newHistory = do
  q <- newTQueueIO
  return (History q)

appendHistory :: History -> HistEvent -> IO ()
appendHistory (History q) ev = atomically (writeTQueue q ev)

readHistory :: History -> IO [HistEvent]
readHistory (History q) = atomically (flushTQueue q)

------------------------------------------------------------------------

blackboxHistory :: forall req resp. (Typeable req, Typeable resp)
                => [HistEvent] -> History' req resp
blackboxHistory = Lec2.History . go []
  where
    go :: [Operation' req resp] -> [HistEvent] -> [Operation' req resp]
    go acc [] = reverse acc
    go acc (HistEvent _nodeId _state input _state' outputs : evs) =
      go (clientRequest input ++ foldMap clientResponse outputs ++ acc) evs

    clientRequest :: Typeable req' => Input req' msg -> [Operation' req resp]
    clientRequest (ClientRequest _time clientId req) = case cast req of
      Just req' -> [Invoke (clientIdToPid clientId) req']
      Nothing   -> []
    clientRequest _otherwise = []

    clientResponse :: Typeable resp' => Output resp' msg -> [Operation' req resp]
    clientResponse (ClientResponse clientId resp) = case cast resp of
      Just resp' -> [Ok (clientIdToPid clientId) resp']
      Nothing    -> []
    clientResponse _otherwise = []

    clientIdToPid :: ClientId -> Pid
    clientIdToPid (ClientId cid) = Pid cid
