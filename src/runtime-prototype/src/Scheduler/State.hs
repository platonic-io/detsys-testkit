module Scheduler.State where

import Data.Map (Map)
import qualified Data.Map as Map

import Scheduler.Agenda (Agenda)
import qualified Scheduler.Agenda as Agenda
import StuntDouble.Random
import StuntDouble.Time

------------------------------------------------------------------------

data SchedulerState = SchedulerState
  { agenda  :: Agenda
  , time    :: Time
  , seed    :: Seed
  , steps   :: Int
  , testId  :: Maybe Int
  , runId   :: Maybe Int
  , clients :: Map String Time
  }

initState :: Time -> Seed -> SchedulerState
initState t s = SchedulerState
  { agenda  = Agenda.empty
  , time    = t
  , seed    = s
  , steps   = 0
  , testId  = Nothing
  , runId   = Nothing
  , clients = Map.empty
  }

lookupClient :: String -> SchedulerState -> Maybe Time
lookupClient c = Map.lookup c . clients
