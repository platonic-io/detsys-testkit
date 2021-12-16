{-# LANGUAGE DeriveGeneric #-}

--XXX we might want to move this module
module Scheduler.Faults where
--- stolen from module Ldfi.Marshal.Faults

import Data.Aeson
import Data.Char (toLower)
import qualified Data.Text.Encoding as Text
import GHC.Generics (Generic)
import Database.SQLite.Simple
import qualified Data.Time as Time

import StuntDouble.IO
import StuntDouble.Time
import StuntDouble.LogicalTime

data Fault
  = Omission
    { from :: String
    , to :: String
    , at :: LogicalTimeInt
    }
  | Crash
    { from :: String
    , at :: LogicalTimeInt
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
    , fromTime :: Time
    , toTime :: Time
    }
  | ClockSkewBump
    { node :: String
    , bumpDuration :: Time.NominalDiffTime
    , fromTime :: Time
    , toTime :: Time
    }
  | ClockSkewStrobe
    { node :: String
    , strobeDelta :: Time.NominalDiffTime
    , strobePeriod :: Time.NominalDiffTime
    , fromTime :: Time
    , toTime :: Time
    }
  | RestartReactor
    { node :: String
    , fromTime :: Time
    }
  | DuplicateMessage
    { node :: String
    , fromTime :: Time
    , toTime :: Time
    , bumpDuration :: Time.NominalDiffTime
    , randomPoint :: Double
    , randomRange :: Double
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
