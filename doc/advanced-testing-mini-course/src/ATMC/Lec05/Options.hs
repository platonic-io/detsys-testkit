module ATMC.Lec05.Options where

import ATMC.Lec05.Deployment

------------------------------------------------------------------------

data Options = Options
  { oDeployment :: DeploymentMode
  -- , oTimerFreq :: Double -- Hz (cycles per second)
  -- oClientTimeoutNanos :: Int
  }
