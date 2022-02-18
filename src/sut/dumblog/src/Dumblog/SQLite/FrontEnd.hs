module Dumblog.SQLite.FrontEnd where

import Control.Concurrent.MVar (newEmptyMVar, takeMVar)
import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TBQueue (TBQueue, writeTBQueue)
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
      let ix = parseIndex
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
    parseIndex :: Int
    parseIndex =
      case pathInfo req of
        [t] -> case decimal t of
          Right (ix, _rest) -> ix
          _otherwise -> error "parseIndex: GET /:ix, :ix isn't an integer"
        _otherwise   -> error "parseIndex: GET /:ix, :ix missing"

runFrontEnd :: TBQueue Command -> Port -> IO ()
runFrontEnd queue port = run port (httpFrontend queue)
