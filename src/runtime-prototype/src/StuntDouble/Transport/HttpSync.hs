{-# LANGUAGE OverloadedStrings #-}

module StuntDouble.Transport.HttpSync where

import Control.Monad
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as LBS
import Network.HTTP.Client

import StuntDouble.Codec
import StuntDouble.Envelope
import StuntDouble.Message
import StuntDouble.Queue
import StuntDouble.Reference
import StuntDouble.Transport

------------------------------------------------------------------------

httpSyncTransport :: Codec -> IO (Transport IO)
httpSyncTransport codec = do
  -- NOTE: This queue doesn't need to be synchronised, as it only has a single
  -- writer at any time. The queue grows if `transportSend`s are made but no
  -- `transportReceive` are made to retrieve their replies.
  queue <- newQueue 128
  manager <- newManager defaultManagerSettings
  return Transport { transportSend = transportSyncSend manager codec queue
                   , transportReceive = dequeue queue
                   , transportShutdown = return ()
                   }

transportSyncSend :: Manager -> Codec -> Queue Envelope -> Envelope -> IO ()
transportSyncSend manager codec@(Codec _encode decode) queue e = do
  request <- envelopeToRequestSync codec e
  -- XXX: Instead of sending right away here, we could batch instead and only
  -- send ever 10 ms or whatever, we could also send concurrently (we would need
  -- to asynchronously take care of possible errors though). Some care needs to
  -- be taking with regards to the unsynchronised queue if this is done.
  resp <- responseBody <$> httpLbs request manager
  case decode resp of
    Left err -> error ("transportSend: couldn't parse response: " ++ show err)
    Right envelope -> do
      ok <- enqueue envelope queue
      unless ok (error "transportSend: couldn't enqueue response...")

envelopeToRequestSync :: Codec -> Envelope -> IO Request
envelopeToRequestSync (Codec encode _decode) e = do
  let Encode address corrId payload = encode e

  initialRequest <- parseRequest address

  return initialRequest
           { method      = "POST"
           , requestBody = RequestBodyLBS payload
           , requestHeaders = requestHeaders initialRequest ++
                              [("correlation-id", BS.pack (show corrId))]
           }
