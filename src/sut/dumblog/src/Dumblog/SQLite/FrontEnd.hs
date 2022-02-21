{-# LANGUAGE OverloadedStrings #-}

module Dumblog.SQLite.FrontEnd where

import Control.Concurrent.MVar (newEmptyMVar, takeMVar)
import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TBQueue (TBQueue, writeTBQueue)
import Data.ByteString.Lazy.Char8 (ByteString)
import qualified Data.ByteString.Lazy.Char8 as BSChar8
import Data.Text.Read (decimal)
import Network.HTTP.Types.Status (status200, status400)
import Network.Wai
       ( Application
       , consumeRequestBodyStrict
       , pathInfo
       , requestMethod
       , responseLBS
       )
import Network.Wai.Handler.Warp (Port, run)

import Dumblog.SQLite.Command

------------------------------------------------------------------------

httpFrontend :: TBQueue Command -> Application
httpFrontend queue req respond =
  case requestMethod req of
    "GET" -> do
      case parseIndex of
        Left err ->
          respond (responseLBS status400 [] err)
        Right ix -> do
          response <- newEmptyMVar
          atomically (writeTBQueue queue (Read ix response))
          bs <- takeMVar response
          respond (responseLBS status200 [] bs)
    "POST" -> do
      bs <- consumeRequestBodyStrict req
      response <- newEmptyMVar
      atomically (writeTBQueue queue (Write bs response))
      ix <- takeMVar response
      respond (responseLBS status200 [] (BSChar8.pack (show ix)))
  where
    parseIndex :: Either ByteString Int
    parseIndex =
      case pathInfo req of
        [txt] -> case decimal txt of
          Right (ix, _rest) -> Right ix
          _otherwise -> Left (BSChar8.pack "parseIndex: GET /:ix, :ix isn't an integer")
        _otherwise   -> Left (BSChar8.pack "parseIndex: GET /:ix, :ix missing")

runFrontEnd :: TBQueue Command -> Port -> IO ()
runFrontEnd queue port = run port (httpFrontend queue)
