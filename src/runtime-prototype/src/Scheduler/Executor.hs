{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NumericUnderscores #-}

module Scheduler.Executor where

import Data.Aeson as Aeson
import Data.ByteString.Lazy.Char8 (ByteString)
import qualified Data.ByteString.Lazy.Char8 as LBS
import Data.Char (toLower)
import Data.Ratio ((%))
import GHC.Generics (Generic)

import Scheduler.Event
import StuntDouble

------------------------------------------------------------------------

data ExecutorResponse = ExecutorResponse
  { events :: [UnscheduledEvent]
  , corrId :: CorrelationId
  }
  deriving (Generic, Show)

instance FromJSON ExecutorResponse

data UnscheduledEvent = UEMessage
  { ueEvent :: String
  , ueArgs  :: Aeson.Value
  , ueTo    :: [String]
  , ueFrom  :: String
  } |
  UEOk
  { ueEvent :: String
  , ueArgs  :: Aeson.Value
  , ueTo    :: [String]
  , ueFrom  :: String
  } |
  UETimer
  { ueArgs :: Aeson.Value
  , ueFrom :: String
  , ueDuration_ns :: Int
  }
  deriving (Generic, Eq, Ord, Show)

instance FromJSON UnscheduledEvent where
  parseJSON = genericParseJSON defaultOptions
    { fieldLabelModifier = \s -> kebabify $ case drop (length ("ue" :: String)) s of
        (x : xs) -> toLower x : xs
        [] -> error "parseJSON: impossible, unless the field names of `UnscheduledEvent` changed"
    , sumEncoding = defaultTaggedObject
      { tagFieldName = "kind"}
    , constructorTagModifier = \s -> case drop (length ("UE" :: String)) s of
        (x : xs) -> toLower x : xs
        [] -> error "parseJSON: impossible, unless the constructor names of `UnscheduledEvent` changed"
    }
    where
      kebabify = map (\x -> if x == '_' then '-' else x)

toScheduled :: Time -> UnscheduledEvent -> [SchedulerEvent]
toScheduled at (UEMessage event args tos from)
  = [ SchedulerEvent "message" event args to from at Nothing | to <- tos]
toScheduled at (UEOk event args tos from)
  = [ SchedulerEvent "ok" event args to from at Nothing | to <- tos]
toScheduled at (UETimer args from duration)
  = [ SchedulerEvent "timer" "timer" args from from at' Nothing]
  where
    at' = addTime at duration'
    duration' = fromRational $ fromIntegral duration % 1_000_000_000

data ExecutorEnvelopeMessage = ExecutorEnvelopeMessage
  { executorEnvelopeMessageKind :: String
  , executorEnvelopeMessageMessage :: Aeson.Value
  } deriving (Generic)

executorEnvelopeMessageOptions = defaultOptions
  { fieldLabelModifier = \s -> map toLower $ drop (length ("executorEnvelopeMessage" :: String)) s}

instance ToJSON ExecutorEnvelopeMessage where
  toJSON = genericToJSON executorEnvelopeMessageOptions
instance FromJSON ExecutorEnvelopeMessage where
  parseJSON = genericParseJSON executorEnvelopeMessageOptions

data ExecutorEnvelope = ExecutorEnvelope
  { executorEnvelopeKind          :: EnvelopeKind
  , executorEnvelopeSender        :: RemoteRef
  , executorEnvelopeMessage       :: ExecutorEnvelopeMessage
  , executorEnvelopeReceiver      :: RemoteRef
  , executorEnvelopeCorrelationId :: CorrelationId
  , executorEnvelopeLogicalTime   :: Int -- we don't need to send the name part
  }
  deriving (Generic)

executorEnvelopeOptions = defaultOptions
  { fieldLabelModifier = \s -> 'e' : drop (length ("executorE" :: String)) s}

instance ToJSON ExecutorEnvelope where
  toJSON = genericToJSON executorEnvelopeOptions
instance FromJSON ExecutorEnvelope where
  parseJSON = genericParseJSON executorEnvelopeOptions

toExecutorEnvelope :: Envelope -> ExecutorEnvelope
toExecutorEnvelope e = ExecutorEnvelope
  { executorEnvelopeKind          = envelopeKind e
  , executorEnvelopeSender        = envelopeSender e
  , executorEnvelopeMessage       =
    case envelopeMessage e of
      InternalMessage' kind msg -> ExecutorEnvelopeMessage kind msg
      msg -> error $ "Unknown message type: " <> show msg
  , executorEnvelopeReceiver      = envelopeReceiver e
  , executorEnvelopeCorrelationId = envelopeCorrelationId e
  , executorEnvelopeLogicalTime   = let LogicalTime _ i = envelopeLogicalTime e in i
  }

fromExecutorEnvelope :: ExecutorEnvelope -> Envelope
fromExecutorEnvelope e = Envelope
  { envelopeKind          = executorEnvelopeKind e
  , envelopeSender        = executorEnvelopeSender e
  , envelopeMessage       = InternalMessage' "Events" $ (executorEnvelopeMessageMessage $ executorEnvelopeMessage e)
  , envelopeReceiver      = executorEnvelopeReceiver e
  , envelopeCorrelationId = executorEnvelopeCorrelationId e
  , envelopeLogicalTime   = LogicalTime "executor" $ executorEnvelopeLogicalTime e
  }


executorCodec :: Codec
executorCodec = Codec encode decode
  where
    encode :: Envelope -> Encode
    encode e = Encode (address (envelopeReceiver e))
                      (getCorrelationId (envelopeCorrelationId e))
                      (Aeson.encode $ toExecutorEnvelope e)

    decode :: ByteString -> Either String Envelope
    decode bs = case eitherDecode bs of
      Right x -> Right (fromExecutorEnvelope x)
      Left err -> error err
