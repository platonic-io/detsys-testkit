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
import qualified Lec04.LineariseWithFault as Lec4
import Lec05.StateMachine

------------------------------------------------------------------------

newtype History = History (TQueue HistEvent')

data Dropped = DidDrop | DidArrive

data HistEvent' = HistEvent'
  { heDropped :: Dropped
  , heEvent :: HistEvent
  }

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

appendHistory :: History -> Dropped -> HistEvent -> IO ()
appendHistory (History q) dropped ev = atomically (writeTQueue q (HistEvent' dropped ev))

readHistory :: History -> IO [HistEvent']
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
      Nothing   -> error "Failed cast, can't add clientRequest"
    clientRequest _otherwise = []

    clientResponse :: Typeable resp' => Output resp' msg -> [Operation' req resp]
    clientResponse (ClientResponse clientId resp) = case cast resp of
      Just resp' -> [Ok (clientIdToPid clientId) resp']
      Nothing    -> error "Failed cast, can't add clientResponse"
    clientResponse _otherwise = []

    clientIdToPid :: ClientId -> Pid
    clientIdToPid (ClientId cid) = Pid cid

blackboxFailHistory :: forall req resp. (Typeable req, Typeable resp)
                    => [HistEvent] -> Lec4.History' req resp
blackboxFailHistory he = case blackboxHistory he of
  Lec2.History ops -> Lec4.History (fmap go ops)
  where
    go (Lec2.Invoke p c) = Lec4.Invoke p c
    go (Lec2.Ok p r) = Lec4.Ok p r
