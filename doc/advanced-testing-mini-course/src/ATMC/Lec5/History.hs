{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}

module ATMC.Lec5.History where

import Control.Concurrent.STM
import Control.Concurrent.STM.TQueue
import Data.Typeable

import ATMC.Lec2ConcurrentSMTesting (History', Operation'(..), Pid(Pid))
import qualified ATMC.Lec2ConcurrentSMTesting as Lec2
import ATMC.Lec5.StateMachine

------------------------------------------------------------------------

newtype History = History (TQueue HistEvent)

data HistEvent = forall state req msg resp.
  ( Show state, Show req, Show msg, Show resp
  , Typeable req, Typeable resp, Typeable msg
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

blackboxHistory :: forall req resp msg. (Typeable req, Typeable resp, Typeable msg)
                => [HistEvent] -> History' req resp
blackboxHistory = Lec2.History . go []
  where
    go :: [Operation' req resp] -> [HistEvent] -> [Operation' req resp]
    go acc [] = reverse acc
    go acc (HistEvent _nodeId _state input _state' outputs : evs) =
      case (cast input, gcast outputs) of
        (Just input', Just outputs') ->
          go (clientRequest input' ++ concatMap clientResponse outputs' ++ acc) evs
        (Just input', Nothing) ->
          go (clientRequest input' ++ acc) evs
        (Nothing, Just outputs') ->
          go (concatMap clientResponse outputs' ++ acc) evs
        _otherwise -> go acc evs

    clientRequest :: Input req msg -> [Operation' req resp]
    clientRequest (ClientRequest _time clientId req) = [Invoke (clientIdToPid clientId) req]
    clientRequest _otherwise = []

    clientResponse :: Output resp msg -> [Operation' req resp]
    clientResponse (ClientResponse clientId resp) = [Ok (clientIdToPid clientId) resp]
    clientResponse _otherwise = []

    clientIdToPid :: ClientId -> Pid
    clientIdToPid (ClientId cid) = Pid cid
