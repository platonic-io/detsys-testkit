module Scheduler.State where

import Data.Map (Map)
import qualified Data.Map as Map

import Scheduler.Agenda (Agenda)
import qualified Scheduler.Agenda as Agenda
import Scheduler.Fault (FaultState)
import StuntDouble

------------------------------------------------------------------------

data SchedulerState = SchedulerState
  { agenda      :: Agenda
  , faultState  :: FaultState
  , time        :: Time
  , logicalTime :: LogicalTime
  , seed        :: Seed
  , steps       :: Int
  , testId      :: Maybe Int
  , runId       :: Maybe Int
  , clients     :: Map String Time
  }

initState :: Time -> Seed -> SchedulerState
initState t s = SchedulerState
  { agenda      = Agenda.empty
  , faultState  = mempty
  , time        = t
  , logicalTime = LogicalTime (NodeName "scheduler") 0
  , seed        = s
  , steps       = 0
  , testId      = Nothing
  , runId       = Nothing
  , clients     = Map.empty
  }

lookupClient :: String -> SchedulerState -> Maybe Time
lookupClient c = Map.lookup c . clients
