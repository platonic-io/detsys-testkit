{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Lec05.Network where

import Control.Concurrent.MVar
import Control.Exception
import qualified Data.ByteString.Char8 as BS8
import Data.ByteString.Lazy (ByteString)
import Data.Functor
import Data.Text.Read (decimal)
import Network.HTTP.Client
import Network.HTTP.Types.Status
import Network.Wai hiding (requestBody)
import Network.Wai.Handler.Warp
import System.Timeout (timeout)

import Lec05.AwaitingClients
import Lec05.ClientGenerator
import Lec05.Configuration
import Lec05.History
import Lec05.Random
import Lec05.StateMachine
import Lec05.Time
import Lec05.Event
import Lec05.EventQueue

------------------------------------------------------------------------

pORT :: Int
pORT = 8050

data Network = Network
  { nSend    :: NodeId -> NodeId -> ByteString -> IO ()
  , nRespond :: ClientId -> ByteString -> IO ()
  , nRun     :: IO ()
  }

realNetwork :: EventQueue -> Clock -> IO Network
realNetwork evQ clock = do
  ac       <- newAwaitingClients
  mgr      <- newManager defaultManagerSettings
  initReq  <- parseRequest ("http://localhost:" ++ show pORT)
  let sendReq = \_fromNodeId toNodeId msg ->
        initReq { method      = "PUT"
                , path        = path initReq <> BS8.pack (show (unNodeId toNodeId))
                , requestBody = RequestBodyLBS msg
                }
      send from to msg = void (httpLbs (sendReq from to msg) mgr)
                        `catch` (\(e :: HttpException) ->
                                   putStrLn ("send failed, error: " ++ show e))
  return Network
    { nSend    = send
    , nRespond = respondToAwaitingClient ac
    , nRun     = run pORT (app ac clock evQ)
    }

app :: AwaitingClients -> Clock -> EventQueue -> Application
app awaiting clock evQ req respond =
  case requestMethod req of
    "POST" -> case parseNodeId of
                Nothing -> respond (responseLBS status400 [] "Missing receiver node id")
                Just toNodeId -> do
                  reqBody <- consumeRequestBodyStrict req
                  (fromClientId, resp) <- addAwaitingClient awaiting
                  time <- cGetCurrentTime clock
                  eqEnqueue evQ (NetworkEventE
                    (NetworkEvent toNodeId (ClientRequest time fromClientId reqBody)))
                  mBs <- timeout (60_000_000) (takeMVar resp) -- 60s
                  removeAwaitingClient awaiting fromClientId
                  case mBs of
                    Nothing -> do
                      putStrLn "Client response timed out..."
                      respond (responseLBS status500 [] "Timeout due to overload or bug")
                    Just bs -> respond (responseLBS status200 [] bs)
    "PUT" -> case parse2NodeIds of
               Nothing -> respond (responseLBS status400 [] "Missing sender/receiver node id")
               Just (fromNodeId, toNodeId) -> do
                  reqBody <- consumeRequestBodyStrict req
                  time <- cGetCurrentTime clock
                  eqEnqueue evQ (NetworkEventE
                    (NetworkEvent toNodeId (InternalMessage time fromNodeId reqBody)))
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
        _otherwise -> Nothing

fakeNetwork :: EventQueue -> Clock -> Random -> IO Network
fakeNetwork evQ clock random = do
  return Network
    { nSend    = send
    , nRespond = respond
    , nRun     = return ()
    }
  where
    send :: NodeId -> NodeId -> ByteString -> IO ()
    send from to msg = do
      now <- cGetCurrentTime clock
      -- XXX: Exponential distribution?
      d <- randomInterval random (1.0, 20.0) :: IO Double
      let arrivalTime = addTimeSeconds (fromRational (toRational d)) now
      eqEnqueue evQ
        (NetworkEventE (NetworkEvent to (InternalMessage arrivalTime from msg)))

    respond :: ClientId -> ByteString -> IO ()
    respond _clientId _resp = return ()

data NetworkFaults = NetworkFaults
  { nfChanceOfDrop :: Double }

faultyNetwork :: EventQueue -> Clock -> Random -> Configuration
  -> History -> Maybe NetworkFaults -> ClientGenerator -> IO Network
faultyNetwork evQ clock random config history mnf cg = do
  return Network
    { nSend    = send
    , nRespond = respond
    , nRun     = return ()
    }
  where
    probOfDrop = case mnf of
      Nothing -> 0.0
      Just nf -> nfChanceOfDrop nf
    send :: NodeId -> NodeId -> ByteString -> IO ()
    send from to msg = do
      now <- cGetCurrentTime clock
      willDrop <- (< probOfDrop) <$> randomInterval random (0.0, 1.0)
      if willDrop
        then do
          mst <- lookupReceiver to config
          case mst of
            Nothing -> error $ "Can't find actor: " <> show to
            Just (SomeCodecSM _ sm) -> do
             let
               st = smState sm
               he = HistEvent to st (InternalMessage now from msg :: Input () ByteString) st ([] :: [Output () ByteString])
             appendHistory history DidDrop he
        else do
          -- XXX: Exponential distribution?
          d <- randomInterval random (1.0, 20.0) :: IO Double
          let arrivalTime = addTimeSeconds (fromRational (toRational d)) now
          eqEnqueue evQ
            (NetworkEventE (NetworkEvent to (InternalMessage arrivalTime from msg)))

    respond :: ClientId -> ByteString -> IO ()
    respond clientId resp = cgRespond cg clientId resp
