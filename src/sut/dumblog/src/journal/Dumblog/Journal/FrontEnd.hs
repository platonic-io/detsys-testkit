{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Dumblog.Journal.FrontEnd where

import Control.Concurrent.MVar (MVar, putMVar)
import Data.Binary (encode)
import qualified Data.ByteString.Char8 as BS8
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy.Char8 as LBS8
import Data.Int (Int64)
import Data.Text.Read (decimal)
import Network.HTTP.Types.Status (status200, status400, status404)
import qualified Network.Wai as Wai
import Network.Wai.Handler.Warp
import System.Timeout (timeout)

import Journal.Internal.Metrics (incrCounter)
import qualified Journal.MP as Journal
import Journal.Types (Journal)

import Dumblog.Common.Metrics
import Dumblog.Common.Types
import Dumblog.Journal.Blocker
import Dumblog.Journal.Codec
import Dumblog.Journal.Types

------------------------------------------------------------------------

data FrontEndInfo = FrontEndInfo
  { blockers :: Blocker ClientResponse
  , currentVersion :: Int64
  }

httpFrontend :: Journal -> DumblogMetrics -> FrontEndInfo -> Wai.Application
httpFrontend journal metrics (FrontEndInfo blocker cVersion) req respond = do
  case Wai.requestMethod req of
    "GET" -> do
      case parseIndex of
        Left err -> do
          incrCounter metrics ErrorsEncountered 1
          respond (Wai.responseLBS status400 [] err)
        Right ix -> appendInputWaitForResp (ClientRequest (Read ix))
    "POST" -> do
      reqBody <- Wai.consumeRequestBodyStrict req
      appendInputWaitForResp (ClientRequest (Write reqBody))
    "PUT" -> do
      case parseIndex of
        Left err -> do
          incrCounter metrics ErrorsEncountered 1
          respond (Wai.responseLBS status400 [] err)
        Right ix  -> do
          reqBody <- Wai.consumeRequestBodyStrict req
          case parseSeqNum of
            Nothing  -> respond (Wai.responseLBS status400 [] "Missing sequence number")
            Just sn ->
              if not (LBS8.null reqBody)
              then appendInputNoWaitForResp (InternalMessageIn (Backup ix reqBody sn))
              else appendInputNoWaitForResp (InternalMessageIn (Ack ix sn))
    "CONNECT" -> do
      case parseIndex of
        Left err -> do
          incrCounter metrics ErrorsEncountered 1
          respond (Wai.responseLBS status400 [] err)
        Right ix -> appendInputNoWaitForResp (AdminCommand (Connect ix))

    _otherwise -> do
      incrCounter metrics ErrorsEncountered 1
      respond (Wai.responseLBS status400 [] "Invalid method")
  where
    parseIndex :: Either ByteString Int
    parseIndex =
      case Wai.pathInfo req of
        [txt] -> case decimal txt of
          Right (ix, _rest) -> Right ix
          _otherwise -> Left (LBS8.pack "parseIndex: GET /:ix, :ix isn't an integer")
        _otherwise   -> Left (LBS8.pack "parseIndex: GET /:ix, :ix missing")

    parseSeqNum :: Maybe SeqNum
    parseSeqNum = do
      key <- lookup "key" (Wai.requestHeaders req)
      fmap (SeqNum . fst) (BS8.readInt key)

    appendInputWaitForResp :: (SeqNum -> Input) -> IO Wai.ResponseReceived
    appendInputWaitForResp input = do
      key <- newKey blocker
      !arrivalTime <- getCurrentNanosSinceEpoch
      let bs = encode (Envelope (input (SeqNum (sequenceNumber key)))
                                cVersion arrivalTime)
          success = do
            incrCounter metrics QueueDepth 1
            blockRespond key
          failure err = do
            cancel blocker key
            incrCounter metrics ErrorsEncountered 1
            respond $ Wai.responseLBS status400 [] (LBS8.pack (show err))
      retryAppendBS bs success failure

    appendInputNoWaitForResp :: Input -> IO Wai.ResponseReceived
    appendInputNoWaitForResp input = do
      !arrivalTime <- getCurrentNanosSinceEpoch
      let bs = encode (Envelope input cVersion arrivalTime)
          success = do
            incrCounter metrics QueueDepth 1
            respond $ Wai.responseLBS status200 [] (LBS8.pack "")
          failure err = do
            incrCounter metrics ErrorsEncountered 1
            respond $ Wai.responseLBS status400 [] (LBS8.pack (show err))
      retryAppendBS bs success failure

    blockRespond key = do
      mResp <- timeout (30*1000*1000) (blockUntil key)
      -- Journal.dumpJournal journal
      case mResp of
        Nothing -> do
          cancel blocker key
          incrCounter metrics ErrorsEncountered 1
          respond $ Wai.responseLBS status400 [] "MVar timeout"
        Just (Error errMsg) -> do
          incrCounter metrics ErrorsEncountered 1
          respond $ Wai.responseLBS status400 [] errMsg
        Just NotFound -> do
          incrCounter metrics ErrorsEncountered 1
          respond $ Wai.responseLBS status404 [] "Not found"
        Just (OK msg) -> respond $ Wai.responseLBS status200 [] msg

    retryAppendBS bs success failure = do
      res <- Journal.appendLBS journal bs
      case res of
        Left err -> do
          putStrLn ("httpFrontend, append error: " ++ show err)
          res' <- Journal.appendLBS journal bs
          case res' of
            Left err' -> do
              putStrLn ("httpFrontend, append error 2: " ++ show err')
              failure err'
            Right () -> success
        Right () -> success

runFrontEnd :: Port -> Journal -> DumblogMetrics -> FrontEndInfo -> Maybe (MVar ()) -> IO ()
runFrontEnd port journal metrics feInfo mReady =
  runSettings settings (httpFrontend journal metrics feInfo)
  where
    settings
      = setPort port
      $ setOnOpen  (\_addr -> incrCounter metrics CurrentNumberTransactions 1 >> return True)
      $ setOnClose (\_addr -> incrCounter metrics CurrentNumberTransactions (-1))
                     -- >> putStrLn ("closing: " ++ show addr))
      -- $ setLogger (\req status _mSize ->
      --                 when (status /= status200) $ do
      --                   putStrLn ("warp, request: " ++ show req)
      --                   putStrLn ("warp, status: "  ++ show status)
      --                   print =<< Wai.strictRequestBody req)
      $ setBeforeMainLoop (putStrLn ("Running on port " ++ show port) >>
                           maybe (pure ()) (\ready -> putMVar ready ()) mReady)
      $ defaultSettings
