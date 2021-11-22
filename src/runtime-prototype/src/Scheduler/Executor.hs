{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NumericUnderscores #-}

module Scheduler.Executor where

import Control.Applicative ((<|>))
import Data.Aeson
import Data.Aeson.Types
import Data.Aeson.Parser
import Data.ByteString.Lazy.Char8 (ByteString)
import qualified Data.ByteString.Lazy.Char8 as LBS
import Data.Char (toLower)
import Data.Ratio ((%))
import GHC.Generics (Generic)

import Scheduler.Event
import StuntDouble hiding (Value)

------------------------------------------------------------------------

data ExecutorResponse = ExecutorResponse
  { events :: [UnscheduledEvent]
  , corrId :: CorrelationId
  }
  deriving (Generic, Show)

instance FromJSON ExecutorResponse

data UnscheduledEvent = UEMessage
  { ueEvent :: String
  , ueArgs  :: Value
  , ueTo    :: [String]
  , ueFrom  :: String
  } |
  UEOk
  { ueEvent :: String
  , ueArgs  :: Value
  , ueTo    :: [String]
  , ueFrom  :: String
  } |
  UETimer
  { ueArgs :: Value
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

isOk :: UnscheduledEvent -> Bool
isOk UEOk {}    = True
isOk _otherwise = False

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
executorCodec = Codec enc dec
  where
    enc :: Message -> ByteString
    enc (InternalMessage t v) = encode (object ["kind" .= t, "message" .= v])
    enc msg                   = encode (genericToJSON defaultOptions msg)

    -- XXX: Ideally we want to use:
    -- https://hackage.haskell.org/package/aeson-1.5.5.1/docs/Data-Aeson-Parser.html#v:eitherDecodeWith
    -- which gives an either, but as far as I can see there's no way to create a
    -- type `(Value -> IResult a)` without `iparse`
    -- (https://hackage.haskell.org/package/aeson-1.5.5.1/docs/src/Data.Aeson.Types.Internal.html#iparse)
    -- which is in a hidden module...
    dec :: ByteString -> Either String Message

    dec = maybe
            (Left "executorCodec: failed to decode")
            Right . decodeWith json (parse parseJSON')
      where
        parseJSON' obj = withObject "InternalMessage" (\v -> InternalMessage
                           <$> v .: "kind"
                           <*> v .: "message") obj
                         <|> genericParseJSON defaultOptions obj
