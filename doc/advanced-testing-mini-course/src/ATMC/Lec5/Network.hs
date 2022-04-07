{-# LANGUAGE OverloadedStrings #-}
module ATMC.Lec5.Network where

import Data.Typeable
import Control.Concurrent.MVar
import Control.Concurrent.STM
import Control.Concurrent.STM.TBQueue
import qualified Data.ByteString.Char8 as BS8
import Data.ByteString.Lazy (ByteString)
import Data.Functor
import Data.Text.Read (decimal)
import Data.Typeable
import Network.HTTP.Client
import Network.HTTP.Types.Status
import Network.Wai hiding (requestBody)
import Network.Wai.Handler.Warp

import ATMC.Lec5.AwaitingClients
import ATMC.Lec5.EventQueue
import ATMC.Lec5.Options
import ATMC.Lec5.StateMachine
import ATMC.Lec5.Time

------------------------------------------------------------------------

pORT :: Int
pORT = 8050

data Network = Network
  { nRecv :: IO ByteString
  , nSend :: NodeId -> ByteString -> IO ()
  , nRun  :: IO ()
  }

realNetwork :: EventQueue -> AwaitingClients -> Clock -> IO Network
realNetwork queue ac clock = do
  incoming <- newTBQueueIO 65536
  mgr      <- newManager defaultManagerSettings
  initReq  <- parseRequest ("http://localhost:" ++ show pORT)
  let sendReq = \nodeId msg ->
        initReq { method      = "PUT"
                -- , path        = path initReq <> BS8.pack (addrToPath addr)
                , requestBody = RequestBodyLBS msg
                }
  return Network
    { nSend = \addr msg -> void (httpLbs (sendReq addr msg) mgr) -- XXX: error handling
    , nRecv = atomically (readTBQueue incoming)
    , nRun  = run pORT (app queue ac clock incoming)
    }

app :: EventQueue -> AwaitingClients -> Clock -> TBQueue ByteString -> Application
app queue awaiting clock incoming req respond =
  case requestMethod req of
    "POST" -> case parseNodeId of
                Nothing -> respond (responseLBS status400 [] "No receiver node id")
                Just nodeId -> do
                  reqBody <- consumeRequestBodyStrict req
                  resp <- newEmptyMVar
                  clientId <- addAwaitingClient awaiting
                  time <- cGetCurrentTime clock
                  enqueueEvent queue
                    (NetworkEvent (RawInput nodeId
                                   (ClientRequest time clientId reqBody)))
                  bs <- takeMVar resp
                  respond (responseLBS status200 [] bs)
    "PUT" -> error "internal msg"
    _otherwise -> respond (responseLBS status400 [] "Unsupported method")
  where
    parseNodeId :: Maybe NodeId
    parseNodeId =
      case pathInfo req of
        [txt] -> case decimal txt of
          Right (nodeId, _rest) -> Just (NodeId nodeId)
          _otherwise -> Nothing
        _otherwise   -> Nothing

fakeNetwork :: EventQueue -> AwaitingClients -> Clock -> IO Network
fakeNetwork = undefined

newNetwork :: Deployment -> EventQueue -> AwaitingClients -> Clock -> IO Network
newNetwork Production = realNetwork
newNetwork Simulation = fakeNetwork
