{-# LANGUAGE OverloadedStrings #-}

module Dumblog.Common.HttpClient where

import Control.Exception (try)
import qualified Data.ByteString.Char8 as BS8
import Data.ByteString.Lazy.Char8 (ByteString)
import qualified Data.ByteString.Lazy.Char8 as LBS8
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
       , requestHeaders
       , responseBody
       , responseTimeoutMicro
       )
import Network.Wai.Handler.Warp (Port)
import Text.Read (readMaybe)

import Dumblog.Common.Types (SeqNum, unSeqNum)

------------------------------------------------------------------------

data HttpClient = HttpClient
  { hcManager   :: Manager -- | NOTE: If possible, you should share a single
                           -- `Manager` between multiple threads and requests.
  , hcWriteReq  :: ByteString -> Request
  , hcReadReq   :: Int -> Request
  , hcBackupReq :: Int -> ByteString -> SeqNum -> Request
  , hcAckReq    :: Int -> SeqNum -> Request
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
                           , path   = path initReq <> BS8.pack (show ix)
                           }

      backupReq :: Int -> ByteString -> SeqNum -> Request
      backupReq ix bs sn = initReq
        { method         = "PUT"
        , path           = path initReq <> BS8.pack (show ix)
        , requestBody    = RequestBodyLBS bs
        , requestHeaders = requestHeaders initReq ++ [("key", BS8.pack (show (unSeqNum sn)))]
        }

      ackReq :: Int -> SeqNum -> Request
      ackReq ix sn = initReq
        { method         = "PUT"
        , path           = path initReq <> BS8.pack (show ix)
        , requestHeaders = requestHeaders initReq ++ [("key", BS8.pack (show (unSeqNum sn)))]
        }

  return (HttpClient mgr writeReq readReq backupReq ackReq)

writeHttp :: HttpClient -> ByteString -> IO (Maybe Int)
writeHttp hc bs = do
  eResp <- try (httpLbs (hcWriteReq hc bs) (hcManager hc))
  case eResp of
    Left (HttpExceptionRequest _req exceptCtx) -> do
      -- XXX: increment hcErrors
      putStrLn ("writeHttp, exception context: " ++ show exceptCtx)
      return Nothing
    Left InvalidUrlException {} -> error "writeHttp, impossible: invalid url"
    Right resp -> return (readMaybe (LBS8.unpack (responseBody resp)))

readHttp :: HttpClient -> Int -> IO (Maybe ByteString)
readHttp hc ix = do
  eResp <- try (httpLbs (hcReadReq hc ix) (hcManager hc))
  case eResp of
    Left (HttpExceptionRequest _req exceptCtx) -> do
      putStrLn ("readHttp, exception context: " ++ show exceptCtx)
      return Nothing -- XXX: increment hcErrors
    Left InvalidUrlException {} -> error "readHttp, impossible: invalid url"
    Right resp -> return (Just (responseBody resp))

backupHttp :: HttpClient -> Int -> ByteString -> SeqNum -> IO ()
backupHttp hc ix bs sn = do
  eResp <- try (httpLbs (hcBackupReq hc ix bs sn) (hcManager hc))
  case eResp of
    Left (HttpExceptionRequest _req exceptCtx) -> do
      putStrLn ("backupHttp, exception context: " ++ show exceptCtx)
    Left InvalidUrlException {} -> error "backupHttp, impossible: invalid url"
    Right _resp -> return ()

ackHttp :: HttpClient -> Int -> SeqNum -> IO ()
ackHttp hc ix sn = do
  eResp <- try (httpLbs (hcAckReq hc ix sn) (hcManager hc))
  case eResp of
    Left (HttpExceptionRequest _req exceptCtx) -> do
      putStrLn ("ackHttp, exception context: " ++ show exceptCtx)
    Left InvalidUrlException {} -> error "ackHttp, impossible: invalid url"
    Right _resp -> return ()
