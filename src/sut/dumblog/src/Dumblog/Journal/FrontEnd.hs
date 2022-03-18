{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Dumblog.Journal.FrontEnd where

import Control.Concurrent.MVar (MVar, putMVar)
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Lazy.Char8 as LBS8
import Data.Int (Int64)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Data.Text.Read as TextReader
import Network.HTTP.Types.Method
import Network.HTTP.Types.Status (status200, status400, status404)
import qualified Network.Wai as Wai
import Network.Wai.Handler.Warp
import System.Timeout (timeout)

import Journal.Internal.Metrics (incrCounter)
import qualified Journal.MP as Journal
import Journal.Types (Journal)

import Dumblog.Common.Metrics
import Dumblog.Journal.Blocker
import Dumblog.Journal.Codec
import Dumblog.Journal.Types

------------------------------------------------------------------------

data FrontEndInfo = FrontEndInfo
  { blockers :: Blocker Response
  , currentVersion :: Int64
  }

httpFrontend :: Journal -> DumblogMetrics -> FrontEndInfo -> Wai.Application
httpFrontend journal metrics (FrontEndInfo blocker cVersion) req respond = do
  body <- Wai.strictRequestBody req
  let mmethod = case parseMethod $ Wai.requestMethod req of
        Left err -> Left $ LBS.fromStrict err
        Right GET -> case Wai.pathInfo req of
          [it] -> case TextReader.decimal it of
            Left err -> Left $ LBS8.pack err
            Right (i, t)
              | Text.null t -> Right $ Read i
              | otherwise -> Left $ "Couldn't parse `"
                 <> LBS.fromStrict (Text.encodeUtf8 it)
                 <> "` as an integer"
          _ -> Left "Path need to have transaction index"
        Right POST -> Right $ Write (LBS.toStrict body)
        _ -> Left $ "Unknown method type require GET/POST"
  case mmethod of
    Left err -> do
      incrCounter metrics ErrorsEncountered 1
      respond $ Wai.responseLBS status400 [] err
    Right cmd -> do
      key <- newKey blocker
      now <- getCurrentNanosSinceEpoch
      let env = encode (Envelope (sequenceNumber key) cmd cVersion now)
      res <- Journal.appendBS journal env
      res' <- case res of
        Left err -> do
          putStrLn ("httpFrontend, append error: " ++ show err)
          Journal.appendBS journal env
        Right () -> return (Right ())
      case res' of
        Left err -> do
          putStrLn ("httpFrontend, append error 2: " ++ show err)
          cancel blocker key
          incrCounter metrics ErrorsEncountered 1
          respond $ Wai.responseLBS status400 [] (LBS8.pack (show err))
        Right () -> do
          incrCounter metrics QueueDepth 1
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

runFrontEnd :: Port -> Journal -> DumblogMetrics -> FrontEndInfo -> Maybe (MVar ()) -> IO ()
runFrontEnd port journal metrics feInfo mReady =
  runSettings settings (httpFrontend journal metrics feInfo)
  where
    settings
      = setPort port
      $ setOnOpen  (\_addr -> incrCounter metrics CurrentNumberTransactions 1 >> return True)
      $ setOnClose (\_addr  -> incrCounter metrics CurrentNumberTransactions (-1))
                     -- >> putStrLn ("closing: " ++ show addr))
      -- $ setLogger (\req status _mSize ->
      --                 when (status /= status200) $ do
      --                   putStrLn ("warp, request: " ++ show req)
      --                   putStrLn ("warp, status: "  ++ show status)
      --                   print =<< Wai.strictRequestBody req)
      $ maybe id (\ready -> setBeforeMainLoop (putMVar ready ())) mReady
      $ defaultSettings
