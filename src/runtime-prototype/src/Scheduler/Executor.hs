{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Scheduler.Executor where

import Data.Aeson
import Data.ByteString.Lazy.Char8 (ByteString)
import qualified Data.ByteString.Lazy.Char8 as LBS
import Data.Char (toLower)
import Data.Time
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
  , ueArgs  :: Data.Aeson.Value
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

fromSDatatype :: UTCTime -> SDatatype -> Maybe [SchedulerEvent]
fromSDatatype at (SList
  [SString kind, SString event, SValue args, SList tos, SString from])
  = Just [ SchedulerEvent kind event args to from at Nothing | SString to <- tos ]
fromSDatatype _at _d = Nothing

executorCodec :: Codec
executorCodec = Codec encode decode
  where
    encode :: Envelope -> Encode
    encode e = Encode (address (envelopeReceiver e))
                      (getCorrelationId (envelopeCorrelationId e))
                      (LBS.pack (getMessage (envelopeMessage e)))

    decode :: ByteString -> Either String Envelope
    decode bs = case eitherDecode bs of
      Right (ExecutorResponse evs corrId) -> Right $
        Envelope
          { envelopeKind             = ResponseKind
          , envelopeSender           = RemoteRef "executor" 0
          -- XXX: going to sdatatype here seems suboptimal...
          , envelopeMessage          = InternalMessage' "Events" (map toSDatatype evs)
          , envelopeReceiver         = RemoteRef "scheduler" 0
          , envelopeCorrelationId    = corrId
          , envelopeLogicalTimestamp = LogicalTimestamp "executor" (-1)
          }
      Left err -> error err
