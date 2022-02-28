{-# LANGUAGE OverloadedStrings #-}

module Dumblog.Common.HttpClient where

import Control.Monad (when)
import Control.Exception (try)
import qualified Data.ByteString.Char8 as BSChar8
import Data.ByteString.Lazy.Char8 (ByteString)
import qualified Data.ByteString.Lazy.Char8 as LBSChar8
import Network.HTTP.Client
       ( Manager
       , Request
       , RequestBody(RequestBodyLBS)
       , HttpException(HttpExceptionRequest)
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
  eResp <- try (httpLbs (hcWriteReq hc bs) (hcManager hc))
  case eResp of
    Left (HttpExceptionRequest _req _exceptCtx) ->
      -- XXX: increment hcErrors
      return (-1)
    Right resp -> return (read (LBSChar8.unpack (responseBody resp)))

readHttp :: HttpClient -> Int -> IO ByteString
readHttp hc ix = do
  eResp <- try (httpLbs (hcReadReq hc ix) (hcManager hc))
  case eResp of
    Left (HttpExceptionRequest _req _exceptCtx) ->
      return "error" -- XXX: increment hcErrors
    Right resp -> return (responseBody resp)
