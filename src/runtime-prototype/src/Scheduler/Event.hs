{-# LANGUAGE DeriveGeneric #-}

module Scheduler.Event where

import Data.Aeson
import GHC.Generics (Generic)

import StuntDouble.Time

------------------------------------------------------------------------

data SchedulerEvent = SchedulerEvent
  { kind  :: String
  , event :: String
  , args  :: {- JSON -} Value
  , to    :: String
  , from  :: String
  , at    :: Time
  , meta  :: Maybe Meta
  }
  deriving (Generic, Eq, Ord, Show)

instance FromJSON SchedulerEvent
instance ToJSON SchedulerEvent

data Meta = Meta
  { test_id      :: Int
  , run_id       :: Int
  , logical_time :: Int
  }
  deriving (Generic, Eq, Ord, Show)

instance ToJSON Meta where
  toEncoding = genericToEncoding defaultOptions
    -- The executor expects kebab-case.
    { fieldLabelModifier = map (\c -> if c == '_' then '-' else c) }

instance FromJSON Meta where
