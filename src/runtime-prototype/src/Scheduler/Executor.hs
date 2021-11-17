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

executorCodec :: Codec
executorCodec = Codec encode decode
  where
    encode :: Envelope -> Encode
    encode e = Encode (address (envelopeReceiver e))
                      (getCorrelationId (envelopeCorrelationId e))
                      (Aeson.encode e)

    decode :: ByteString -> Either String Envelope
    decode bs = case eitherDecode bs of
      Right x -> Right x
      Left err -> error err
