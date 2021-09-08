module StuntDouble.Transport.HttpSync where

import Control.Monad
import Data.Aeson
import Network.HTTP.Client

import StuntDouble.Envelope
import StuntDouble.Queue
import StuntDouble.Transport
import StuntDouble.Transport.Http (envelopeToRequest)

------------------------------------------------------------------------

httpSyncTransport :: IO (Transport IO)
httpSyncTransport = do
  -- NOTE: This queue doesn't need to be synchronised, as it only has a single
  -- writer at any time. The queue grows if `transportSend`s are made but no
  -- `transportReceive` are made to retrieve their replies.
  queue <- newQueue 128
  manager <- newManager defaultManagerSettings
  return Transport { transportSend = transportSyncSend manager queue
                   , transportReceive = dequeue queue
                   , transportShutdown = return ()
                   }

transportSyncSend :: Manager -> Queue Envelope -> Envelope -> IO ()
transportSyncSend manager queue e = do
  request <- envelopeToRequest e
  -- XXX: Instead of sending right away here, we could batch instead and only
  -- send ever 10 ms or whatever, we could also send concurrently (we would need
  -- to asynchronously take care of possible errors though). Some care needs to
  -- be taking with regards to the unsynchronised queue if this is done.
  resp <- responseBody <$> httpLbs request manager
  case eitherDecode resp of
    Left err -> error ("transportSend: couldn't parse response: " ++ show err)
    Right envelope -> do
      ok <- enqueue envelope queue
      unless ok (error "transportSend: couldn't enqueue response...")
