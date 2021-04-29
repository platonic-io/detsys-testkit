module Scheduler.Trace where

import Scheduler.Json (Json)
import Scheduler.Time (Timestamp)

import Scheduler.Types

data NetworkTrace = NetworkTrace
  { ntMessage :: String
  , ntArgs :: Json
  , ntFrom :: String
  , ntTo :: String
  , ntKind :: String
  , ntSentLogicalTime :: Int
  , ntRecvLogicalTime :: Int
  , ntRecvSimulatedTime :: Timestamp
  , ntDropped :: Bool
  , ntJepsenType :: Maybe String
  , ntJepsenProcess :: Maybe Int
  }

data Capability m = Capability
  { emitEvent :: TestId -> RunId -> NetworkTrace -> m ()
  }
