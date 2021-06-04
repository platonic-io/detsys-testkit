{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StandaloneDeriving #-}

module StuntDouble.Transport.Http where

import qualified Data.HashMap.Strict as HashMap
import Data.Text (Text)
import GHC.Generics
import Data.Aeson
import Data.Aeson.Parser
import Data.String
import Data.Aeson.Internal
import Control.Concurrent
import Control.Concurrent.STM
import Data.ByteString.Char8 as BS
import Data.ByteString.Lazy as LBS
import Network.HTTP.Client
import Network.HTTP.Types.Status
import qualified Network.Wai as Wai
import Network.Wai.Handler.Warp

import StuntDouble.Envelope
import StuntDouble.Transport
import StuntDouble.Reference
import StuntDouble.Message

------------------------------------------------------------------------

httpTransport :: Port -> IO (Transport IO)
httpTransport port = do
  manager <- newManager defaultManagerSettings
  queue <- newTBQueueIO 128 -- XXX: when/how does this grow?
  _tid <- forkFinally (run port (app queue)) (const (return ()))
  return Transport { transportSend = transportSend' manager
                   , transportReceive = atomically (readTBQueue queue)
                   }

transportSend' :: Manager -> Envelope -> IO ()
transportSend' manager e = do
  request <- envelopeToRequest e
  responseBody <$> httpNoBody request manager

envelopeToRequest :: Envelope -> IO Request
envelopeToRequest e = do

  let url :: String
      url = address (envelopeReceiver e)

      body :: RequestBody
      body = RequestBodyLBS (encode e)

  initialRequest <- parseRequest url

  return initialRequest
           { method      = "POST"
           , requestBody = body
           }

app :: TBQueue Envelope -> Wai.Application
app queue req respond = do
  eEnvelope <- waiRequestToEnvelope req
  case eEnvelope of
    Left err -> do
      respond (Wai.responseLBS status500 [] ("Couldn't parse request: " <> fromString err))
    Right envelope -> do
      atomically (writeTBQueue queue envelope)
      respond (Wai.responseLBS status200 [] LBS.empty)

waiRequestToEnvelope :: Wai.Request -> IO (Either String Envelope)
waiRequestToEnvelope req = do
  body <- Wai.lazyRequestBody req
  return (eitherDecode body)

-- XXX: orphan instances...

deriving instance Generic Envelope
instance ToJSON Envelope
instance FromJSON Envelope

deriving instance Generic EnvelopeKind
instance ToJSON EnvelopeKind
instance FromJSON EnvelopeKind

deriving instance Generic Message
instance ToJSON Message
instance FromJSON Message

deriving instance Generic RemoteRef
instance ToJSON RemoteRef
instance FromJSON RemoteRef

deriving instance Generic CorrelationId
instance ToJSON CorrelationId
instance FromJSON CorrelationId

deriving instance Generic ClientRef
instance ToJSON ClientRef
instance FromJSON ClientRef
