module ATMC.Lec5.Options where

data Deployment = Production | Simulation

data Options = Options
  { oDeployment :: Deployment
  -- , oTimerFreq :: Double -- Hz (cycles per second)
  -- oClientTimeoutNanos :: Int
  }
