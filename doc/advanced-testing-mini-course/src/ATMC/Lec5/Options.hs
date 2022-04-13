module ATMC.Lec5.Options where

import ATMC.Lec5.Agenda
import ATMC.Lec5.StateMachine

------------------------------------------------------------------------

data DeploymentMode = Production | Simulation Agenda
  deriving Show

data Options = Options
  { oDeployment :: DeploymentMode
  -- , oTimerFreq :: Double -- Hz (cycles per second)
  -- oClientTimeoutNanos :: Int
  }
