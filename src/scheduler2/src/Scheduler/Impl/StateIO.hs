module Scheduler.Impl.StateIO where

data RunState = RunState
  { agenda :: Agenda
  , seed :: Seed

  , clock :: Timestamp
  , nextTick :: Timestamp
  , logicalClock :: Int

  -- these are the thorny ones now!
  , clientRequests :: [AgendaEntry]
  }

newtype M a = M {runM :: StateT RunState IO a}
  deriving Monad M

-- could probably be using some lens magic
agendaCapability :: AgendaCapability M
agendaCapability = Agenda.fromModify $ \f -> M $ do
  rs <- get
  let (agenda', res) = f (agenda rs)
  put $ rs {agenda = agenda' }
  return res
