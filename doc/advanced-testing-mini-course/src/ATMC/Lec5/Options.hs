module ATMC.Lec5.Options where

import ATMC.Lec5.Agenda
import ATMC.Lec5.Event
import ATMC.Lec5.StateMachine

------------------------------------------------------------------------

data Deployment = Production | Simulation Agenda
  deriving Show

data Options = Options
  { oDeployment :: Deployment
  -- , oTimerFreq :: Double -- Hz (cycles per second)
  -- oClientTimeoutNanos :: Int
  }
