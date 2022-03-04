{-# LANGUAGE DeriveGeneric #-}
module Debugger.State where

import Data.Aeson
import GHC.Generics (Generic)

import Data.Vector (Vector)

data DebEvent = DebEvent
  { from :: String
  , to :: String
  , event :: String
  , receivedLogical :: Int
  -- , receivedSimulated :: Time
  , message :: String
  } deriving (Generic, Show)

instance FromJSON DebEvent
instance ToJSON DebEvent

data InstanceState = InstanceState
 { isState :: String -- Should probably be per reactor
 , isCurrentEvent :: DebEvent
 , isSeqDia :: String
 , isLogs :: [String]
 , isSent :: [DebEvent]
 }

data InstanceStateRepr = InstanceStateRepr
  { state :: String
  , currentEvent :: DebEvent
  , logs :: [String]
  , sent :: [DebEvent]
  } deriving Generic

instance FromJSON InstanceStateRepr

instance ToJSON InstanceStateRepr

-- this should build the seqDia etc.
fromRepr :: Vector InstanceStateRepr -> Vector InstanceState
fromRepr = fmap repr
  where
    repr :: InstanceStateRepr -> InstanceState
    repr i = InstanceState
      { isState = state i
      , isCurrentEvent = currentEvent i
      , isSeqDia = "Sequence Diagram not supported yet!" -- should actually be built here
      , isLogs = logs i
      , isSent = sent i
      }
