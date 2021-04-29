module Scheduler.Client where

import Scheduler.Time (Timestamp)
  -- , clientRequests :: [AgendaEntry] -- where should these go? Agenda?

data Capability m = Capability
  { isClient :: String -> m (Maybe Int) -- get process id
  , isClientActive :: String -> m (Maybe Timestamp)
  , activateClient :: String -> Timestamp -> m ()
  , deActivateClient :: String -> m ()
  }

colonClient :: String -> Maybe Int
colonClient s = Nothing -- TODO
