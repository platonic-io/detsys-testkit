module Scheduler.State where

import Data.Time

import Scheduler.Agenda (Agenda)
import qualified Scheduler.Agenda as Agenda
import StuntDouble.Random

------------------------------------------------------------------------

data SchedulerState = SchedulerState
  { agenda :: Agenda
  , time   :: UTCTime
  , seed   :: Seed
  , steps  :: Int
  , testId :: Maybe Int
  , runId  :: Maybe Int
  --    , _activeClients :: Map String Timestamp
  }

initState :: UTCTime -> Seed -> SchedulerState
initState t s = SchedulerState
  { agenda = Agenda.empty
  , time   = t
  , seed   = s
  , steps  = 0
  , testId = Nothing
  , runId  = Nothing
  }
