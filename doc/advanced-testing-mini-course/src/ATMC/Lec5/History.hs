{-# LANGUAGE ExistentialQuantification #-}

module ATMC.Lec5.History where

import Control.Concurrent.STM
import Control.Concurrent.STM.TQueue

import ATMC.Lec2ConcurrentSMTesting (History', Operation'(..), Pid(Pid))
import qualified ATMC.Lec2ConcurrentSMTesting as Lec2
import ATMC.Lec5.StateMachine

------------------------------------------------------------------------

newtype History = History (TQueue HistEvent)

data HistEvent = forall state req msg resp.
  HistEvent NodeId state (Input req msg) state [Output resp msg]

newHistory :: IO History
newHistory = do
  q <- newTQueueIO
  return (History q)

appendHistory :: History -> HistEvent -> IO ()
appendHistory (History q) ev = atomically (writeTQueue q ev)

readHistory :: History -> IO [HistEvent]
readHistory (History q) = atomically (flushTQueue q)

------------------------------------------------------------------------

-- XXX: Remove existentials or add typeable constraints...
{-
data JepsenHistory = forall req resp. JepsenHistory (History' resp resp)

blackboxHistory :: [HistEvent] -> JepsenHistory
blackboxHistory = JepsenHistory . Lec2.History . go []
  where
    go acc [] = reverse acc
    go acc (HistEvent _nodeId _state input _state' outputs : evs) =
      go (clientRequest input ++ concatMap clientResponse outputs ++ acc) evs

    clientRequest :: Input req msg -> [Operation' req resp]
    clientRequest (ClientRequest _time clientId req) = [Invoke (clientIdToPid clientId) req]
    clientRequest _otherwise = []

    clientResponse :: Output resp msg -> [Operation' req resp]
    clientResponse (ClientResponse clientId resp) = [Ok (clientIdToPid clientId) resp]
    clientResponse _otherwise = []

    clientIdToPid :: ClientId -> Pid
    clientIdToPid (ClientId cid) = Pid cid

-}
