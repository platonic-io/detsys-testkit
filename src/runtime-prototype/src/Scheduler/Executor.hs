{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Scheduler.Executor where

import Data.Aeson as Aeson
import Data.ByteString.Lazy.Char8 (ByteString)
import qualified Data.ByteString.Lazy.Char8 as LBS
import Data.Char (toLower)
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

data UnscheduledEvent = UnscheduledEvent
  { ueKind  :: String
  , ueEvent :: String
  , ueArgs  :: Aeson.Value
  , ueTo    :: [String]
  , ueFrom  :: String
  }
  deriving (Generic, Eq, Ord, Show)

instance FromJSON UnscheduledEvent where
  parseJSON = genericParseJSON defaultOptions
    { fieldLabelModifier = \s -> case drop (length ("ue" :: String)) s of
        (x : xs) -> toLower x : xs
        [] -> error "parseJSON: impossible, unless the field names of `UnscheduledEvent` changed" }

toSDatatype :: UnscheduledEvent -> SDatatype
toSDatatype (UnscheduledEvent kind event args to from) =
  SList [SString kind, SString event, SValue args, SList (map SString to), SString from]

fromSDatatype :: Time -> SDatatype -> Maybe [SchedulerEvent]
fromSDatatype at (SList
  [SString kind, SString event, SValue args, SList tos, SString from])
  = Just [ SchedulerEvent kind event args to from at Nothing | SString to <- tos ]
fromSDatatype _at _d = Nothing

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
  { fieldLabelModifier = \s -> 'e':drop (length ("executorE" :: String)) s}

instance ToJSON ExecutorEnvelope where
  toJSON = genericToJSON executorEnvelopeOptions
instance FromJSON ExecutorEnvelope where
  parseJSON = genericParseJSON executorEnvelopeOptions

toExecutorEnvelope :: Envelope -> ExecutorEnvelope
toExecutorEnvelope e = ExecutorEnvelope
  { executorEnvelopeKind          = envelopeKind e
  , executorEnvelopeSender        = envelopeSender e
  -- this is silly.. going back and forth between json..
  , executorEnvelopeMessage       =
    let msg = getMessage $ envelopeMessage e in
      case msg of
        "INIT" -> ExecutorEnvelopeMessage "init" ""
        _ -> ExecutorEnvelopeMessage "receive" $ case eitherDecode . LBS.pack $ msg of
          Left err -> error err
          Right x -> x
  , executorEnvelopeReceiver      = envelopeReceiver e
  , executorEnvelopeCorrelationId = envelopeCorrelationId e
  , executorEnvelopeLogicalTime   = let LogicalTime _ i = envelopeLogicalTime e in i
  }

fromExecutorEnvelope :: ExecutorEnvelope -> Envelope
fromExecutorEnvelope e = Envelope
  { envelopeKind          = executorEnvelopeKind e
  , envelopeSender        = executorEnvelopeSender e
  , envelopeMessage       = InternalMessage' "Events" $ case Aeson.fromJSON (executorEnvelopeMessageMessage $ executorEnvelopeMessage e) of
      Aeson.Error err -> error err
      Aeson.Success (ExecutorResponse evs _) -> map toSDatatype evs
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
