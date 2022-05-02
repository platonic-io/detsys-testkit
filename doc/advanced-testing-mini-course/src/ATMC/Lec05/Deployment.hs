module ATMC.Lec05.Deployment where

import Control.Concurrent.Async

import ATMC.Lec05.History
import ATMC.Lec05.Agenda
import ATMC.Lec05.EventQueue
import ATMC.Lec05.Configuration
import ATMC.Lec05.Random
import ATMC.Lec05.Time
import ATMC.Lec05.TimerWheel
import ATMC.Lec05.Network

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
