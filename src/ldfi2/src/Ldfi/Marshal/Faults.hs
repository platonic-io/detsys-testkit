{-# LANGUAGE DeriveGeneric #-}
module Ldfi.Marshal.Faults where

import Data.Aeson
import Data.Char (toLower)
import GHC.Generics (Generic)
import GHC.Natural (Natural)

data Fault
  = Omission
    { from :: String,
      to :: String,
      at :: Natural
    }
  | Crash
    { from :: String,
      at :: Natural
    }
  deriving (Generic, Show)

customOptions :: Options
customOptions = defaultOptions
  { sumEncoding = defaultTaggedObject { tagFieldName = "kind"},
    constructorTagModifier = map toLower
  }
instance FromJSON Fault where
  parseJSON = genericParseJSON customOptions

instance ToJSON Fault where
  toJSON = genericToJSON customOptions
  toEncoding = genericToEncoding customOptions

data Faults = Faults
  { faults :: [Fault]}
  deriving (Generic, Show)

instance ToJSON Faults
