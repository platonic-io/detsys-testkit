{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

module ATMC.Lec5.Network where

import Data.Typeable
import Control.Exception
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
  , nSend :: NodeId -> NodeId -> ByteString -> IO ()
  , nRun  :: IO ()
  }

realNetwork :: EventQueue -> AwaitingClients -> Clock -> IO Network
realNetwork queue ac clock = do
  incoming <- newTBQueueIO 65536
  mgr      <- newManager defaultManagerSettings
  initReq  <- parseRequest ("http://localhost:" ++ show pORT)
  let sendReq = \fromNodeId toNodeId msg ->
        initReq { method      = "PUT"
                , path        = path initReq <> BS8.pack (show (unNodeId toNodeId))
                , requestBody = RequestBodyLBS msg
                }
      send from to msg = void (httpLbs (sendReq from to msg) mgr)
                        `catch` (\(e :: HttpException) ->
                                   putStrLn ("send failed, error: " ++ show e))
  return Network
    { nSend = send
    , nRecv = atomically (readTBQueue incoming)
    , nRun  = run pORT (app queue ac clock incoming)
    }

app :: EventQueue -> AwaitingClients -> Clock -> TBQueue ByteString -> Application
app queue awaiting clock incoming req respond =
  case requestMethod req of
    "POST" -> case parseNodeId of
                Nothing -> respond (responseLBS status400 [] "Missing receiver node id")
                Just receiverNodeId -> do
                  reqBody <- consumeRequestBodyStrict req
                  resp <- newEmptyMVar
                  senderClientId <- addAwaitingClient awaiting
                  time <- cGetCurrentTime clock
                  enqueueEvent queue
                    (NetworkEvent (RawInput receiverNodeId
                                   (ClientRequest time senderClientId reqBody)))
                  bs <- takeMVar resp
                  respond (responseLBS status200 [] bs)
    "PUT" -> case parse2NodeIds of
               Nothing -> respond (responseLBS status400 [] "Missing sender/receiver node id")
               Just (fromNodeId, toNodeId) -> do
                  reqBody <- consumeRequestBodyStrict req
                  time <- cGetCurrentTime clock
                  enqueueEvent queue
                    (NetworkEvent (RawInput toNodeId
                                   (InternalMessage time fromNodeId reqBody)))
                  respond (responseLBS status200 [] "")

    _otherwise -> respond (responseLBS status400 [] "Unsupported method")
  where
    parseNodeId :: Maybe NodeId
    parseNodeId =
      case pathInfo req of
        [txt] -> case decimal txt of
          Right (nodeId, _rest) -> Just (NodeId nodeId)
          _otherwise -> Nothing
        _otherwise   -> Nothing

    parse2NodeIds :: Maybe (NodeId, NodeId)
    parse2NodeIds =
      case pathInfo req of
        [txt, txt'] -> case (decimal txt, decimal txt') of
          (Right (nodeId, _rest), Right (nodeId', _rest')) ->
            Just (NodeId nodeId, NodeId nodeId')
          _otherwise -> Nothing
        _otherwise   -> Nothing

fakeNetwork :: EventQueue -> AwaitingClients -> Clock -> IO Network
fakeNetwork = undefined

newNetwork :: Deployment -> EventQueue -> AwaitingClients -> Clock -> IO Network
newNetwork Production = realNetwork
newNetwork Simulation = fakeNetwork
