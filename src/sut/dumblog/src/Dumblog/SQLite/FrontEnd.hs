{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

module Dumblog.SQLite.FrontEnd where

import Control.Concurrent.MVar (MVar, newEmptyMVar, putMVar, takeMVar)
import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TBQueue (TBQueue, writeTBQueue)
import Data.ByteString.Lazy.Char8 (ByteString)
import qualified Data.ByteString.Lazy.Char8 as BSChar8
import Data.Text.Read (decimal)
import Network.HTTP.Types.Status (status200, status400, status404)
import Network.Wai
       ( Application
       , consumeRequestBodyStrict
       , pathInfo
       , requestMethod
       , responseLBS
       )
import Network.Wai.Handler.Warp
       ( Port
       , defaultSettings
       , runSettings
       , setBeforeMainLoop
       , setOnOpen
       , setOnClose
       , setPort
       )

import Journal.Internal.Metrics
import Dumblog.Common.Metrics
import Dumblog.SQLite.Command

------------------------------------------------------------------------

httpFrontend :: TBQueue Command -> DumblogMetrics -> Application
httpFrontend queue metrics req respond =
  case requestMethod req of
    "GET" -> do
      case parseIndex of
        Left err -> do
          incrCounter metrics ErrorsEncountered 1
          respond (responseLBS status400 [] err)
        Right ix -> do
          response <- newEmptyMVar
          !arrivalTime <- getCurrentNanosSinceEpoch
          atomically (writeTBQueue queue (Read ix arrivalTime response))
          incrCounter metrics QueueDepth 1
          mbs <- takeMVar response
          case mbs of
            Nothing -> do
              incrCounter metrics ErrorsEncountered 1
              respond (responseLBS status404 [] (BSChar8.pack "Not found"))
            Just bs -> respond (responseLBS status200 [] bs)
    "POST" -> do
      bs <- consumeRequestBodyStrict req
      response <- newEmptyMVar
      !arrivalTime <- getCurrentNanosSinceEpoch
      atomically (writeTBQueue queue (Write bs arrivalTime response))
      incrCounter metrics QueueDepth 1
      ix <- takeMVar response
      respond (responseLBS status200 [] (BSChar8.pack (show ix)))
    _otherwise -> do
      incrCounter metrics ErrorsEncountered 1
      respond (responseLBS status400 [] "Invalid method")
  where
    parseIndex :: Either ByteString Int
    parseIndex =
      case pathInfo req of
        [txt] -> case decimal txt of
          Right (ix, _rest) -> Right ix
          _otherwise -> Left (BSChar8.pack "parseIndex: GET /:ix, :ix isn't an integer")
        _otherwise   -> Left (BSChar8.pack "parseIndex: GET /:ix, :ix missing")

runFrontEnd :: TBQueue Command -> DumblogMetrics -> Port -> Maybe (MVar ()) -> IO ()
runFrontEnd queue metrics port mReady = runSettings settings (httpFrontend queue metrics)
  where
    settings
      = setPort port
      $ maybe id (\ready -> setBeforeMainLoop (putMVar ready ())) mReady
      $ setOnOpen  (\_addr -> incrCounter metrics CurrentNumberTransactions 1 >> return True)
      $ setOnClose (\_addr  -> incrCounter metrics CurrentNumberTransactions (-1))
                     -- >> putStrLn ("closing: " ++ show addr))
      $ defaultSettings
