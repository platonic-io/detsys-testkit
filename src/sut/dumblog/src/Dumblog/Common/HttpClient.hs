{-# LANGUAGE OverloadedStrings #-}

module Dumblog.Common.HttpClient where

import Control.Exception (try)
import qualified Data.ByteString.Char8 as BSChar8
import Data.ByteString.Lazy.Char8 (ByteString)
import qualified Data.ByteString.Lazy.Char8 as LBSChar8
import Network.HTTP.Client
       ( HttpException(HttpExceptionRequest, InvalidUrlException)
       , Manager
       , Request
       , RequestBody(RequestBodyLBS)
       , defaultManagerSettings
       , httpLbs
       , managerResponseTimeout
       , method
       , newManager
       , parseRequest
       , path
       , requestBody
       , responseBody
       , responseTimeoutMicro
       )
import Network.Wai.Handler.Warp (Port)
import Text.Read (readMaybe)

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
                      { managerResponseTimeout = responseTimeoutMicro (30 * 1000 * 1000) }
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

writeHttp :: HttpClient -> ByteString -> IO (Maybe Int)
writeHttp hc bs = do
  eResp <- try (httpLbs (hcWriteReq hc bs) (hcManager hc))
  case eResp of
    Left (HttpExceptionRequest _req exceptCtx) -> do
      -- XXX: increment hcErrors
      -- putStrLn ("writeHttp, exception context: " ++ show exceptCtx)
      return Nothing
    Left InvalidUrlException {} -> error "writeHttp, impossible: invalid url"
    Right resp -> return (readMaybe (LBSChar8.unpack (responseBody resp)))

readHttp :: HttpClient -> Int -> IO (Maybe ByteString)
readHttp hc ix = do
  eResp <- try (httpLbs (hcReadReq hc ix) (hcManager hc))
  case eResp of
    Left (HttpExceptionRequest _req exceptCtx) -> do
      putStrLn ("readHttp, exception context: " ++ show exceptCtx)
      return Nothing -- XXX: increment hcErrors
    Left InvalidUrlException {} -> error "readHttp, impossible: invalid url"
    Right resp -> return (Just (responseBody resp))
