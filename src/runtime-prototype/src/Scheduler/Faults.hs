{-# LANGUAGE DeriveGeneric #-}

--XXX we might want to move this module
module Scheduler.Faults where
--- stolen from module Ldfi.Marshal.Faults

import Data.Aeson
import Data.Char (toLower)
import qualified Data.Text.Encoding as Text
import GHC.Generics (Generic)
import Database.SQLite.Simple

import StuntDouble.IO
import StuntDouble.Time

data Fault
  = Omission
      { from :: String,
        to :: String,
        at :: Int
      }
  | Crash
      { from :: String,
        at :: Int
      }
  -- new ones
  | Pause
    { node :: String
    , fromTime :: Time
    , toTime :: Time
    }
  | Partition
    { node :: String
    , cantReceiveFrom :: [String]
    , fromtime :: Time
    , toTime :: Time
    }
  deriving (Generic, Show)

customOptions :: Options
customOptions =
  defaultOptions
    { sumEncoding = defaultTaggedObject {tagFieldName = "kind"},
      constructorTagModifier = map toLower
    }

instance FromJSON Fault where
  parseJSON = genericParseJSON customOptions

instance ToJSON Fault where
  toJSON = genericToJSON customOptions
  toEncoding = genericToEncoding customOptions

data Faults = Faults
  {faults :: [Fault]}
  deriving (Generic, Show)

instance ToJSON Faults

instance ParseRow Faults where
  -- XXX: Text -> ByteString -> JSON, seems unnecessary? We only need the `at`
  -- field for the heap priority, the rest could remain as a text and sent as
  -- such to the executor?
  parseRow [FText t] = case eitherDecodeStrict (Text.encodeUtf8 t) of
    Right es -> Just (Faults es)
    Left err -> error (show err)
  parseRow x         = error (show x)
