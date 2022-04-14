module ATMC.Lec5.Deployment where

import Control.Concurrent.Async

import ATMC.Lec5.History
import ATMC.Lec5.Agenda
import ATMC.Lec5.EventQueue
import ATMC.Lec5.Configuration
import ATMC.Lec5.Random
import ATMC.Lec5.Time
import ATMC.Lec5.TimerWheel
import ATMC.Lec5.Network

------------------------------------------------------------------------

data DeploymentMode = Production | Simulation Seed Agenda History

displayDeploymentMode :: DeploymentMode -> String
displayDeploymentMode Production    = "production"
displayDeploymentMode Simulation {} = "simulation"

data Deployment = Deployment
  { dMode          :: DeploymentMode
  , dConfiguration :: Configuration
  , dClock         :: Clock
  , dEventQueue    :: EventQueue
  , dNetwork       :: Network
  , dTimerWheel    :: TimerWheel
  , dRandom        :: Random
  , dPids          :: Pids
  , dAppendHistory :: HistEvent -> IO ()
  }

newtype Pids = Pids { unPids :: [Async ()] }

newDeployment :: DeploymentMode -> Configuration -> IO Deployment
newDeployment mode config = case mode of
  Production -> do
    clock      <- realClock
    eventQueue <- realEventQueue clock
    network    <- realNetwork eventQueue clock
    timerWheel <- newTimerWheel
    random     <- realRandom
    return Deployment
      { dMode          = mode
      , dConfiguration = config
      , dClock         = clock
      , dEventQueue    = eventQueue
      , dNetwork       = network
      , dTimerWheel    = timerWheel
      , dRandom        = random
      , dPids          = Pids []
      , dAppendHistory = \_ -> return ()
      }
  Simulation seed agenda history -> do
    clock      <- fakeClockEpoch
    eventQueue <- fakeEventQueue agenda clock
    random     <- fakeRandom seed
    network    <- fakeNetwork eventQueue clock random
    timerWheel <- newTimerWheel
    return Deployment
      { dMode          = mode
      , dConfiguration = config
      , dClock         = clock
      , dEventQueue    = eventQueue
      , dNetwork       = network
      , dTimerWheel    = timerWheel
      , dRandom        = random
      , dPids          = Pids []
      , dAppendHistory = appendHistory history
      }
