{-# LANGUAGE OverloadedStrings #-}

module Dumblog.Common.HttpClient where

import Control.Monad (when)
import qualified Data.ByteString.Char8 as BSChar8
import Data.ByteString.Lazy.Char8 (ByteString)
import qualified Data.ByteString.Lazy.Char8 as LBSChar8
import Network.HTTP.Client
       ( Manager
       , Request
       , RequestBody(RequestBodyLBS)
       , defaultManagerSettings
       , httpLbs
       , httpNoBody
       , method
       , newManager
       , parseRequest
       , path
       , requestBody
       , responseBody
       , responseStatus
       )
import Network.HTTP.Types.Status (ok200)
import Network.Wai.Handler.Warp (Port)

------------------------------------------------------------------------

data HttpClient = HttpClient
  { hcManager  :: Manager -- | NOTE: If possible, you should share a single
                          -- `Manager` between multiple threads and requests.
  , hcWriteReq :: ByteString -> Request
  , hcReadReq  :: Int -> Request
  -- , hcErrors   :: AtomicCounter
  }

newHttpClient :: String -> Port -> IO HttpClient
newHttpClient host port = do
  mgr <- newManager defaultManagerSettings
  initReq <- parseRequest ("http://" ++ host ++ ":" ++ show port)

  let writeReq :: ByteString -> Request
      writeReq bs = initReq { method      = "POST"
                            , requestBody = RequestBodyLBS bs
                            }

      readReq :: Int -> Request
      readReq ix = initReq { method = "GET"
                           , path   = path initReq <> BSChar8.pack (show ix)
                           }

  return (HttpClient mgr writeReq readReq)

writeHttp :: HttpClient -> ByteString -> IO Int
writeHttp hc bs = do
  resp <- httpLbs (hcWriteReq hc bs) (hcManager hc)
  when (responseStatus resp /= ok200) $
    return () -- XXX: increment hcErrors
  return (read (LBSChar8.unpack (responseBody resp)))

readHttp :: HttpClient -> Int -> IO ByteString
readHttp hc ix = do
  resp <- httpLbs (hcReadReq hc ix) (hcManager hc)
  when (responseStatus resp /= ok200) $
    return () -- XXX: increment hcErrors
  return (responseBody resp)
