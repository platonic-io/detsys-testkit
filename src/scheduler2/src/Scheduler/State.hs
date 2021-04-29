module Scheduler.State where

-- REMOVE THS FILE

-- we should hide state in each capability?
data RunState = RunState
  { agenda :: Agenda -- ok
  , seed :: Seed -- ok

  , clock :: Timestamp -- ok
  , nextTick :: Timestamp -- remove
  , logicalClock :: Int -- ok

  -- these are the thorny ones now!
  , clientRequests :: [AgendaEntry] -- where should these go? Agenda?
  }
