module ATMC.Lec5.Options where

import ATMC.Lec5.Deployment

------------------------------------------------------------------------

data Options = Options
  { oDeployment :: DeploymentMode
  -- , oTimerFreq :: Double -- Hz (cycles per second)
  -- oClientTimeoutNanos :: Int
  }
