module ATMC.Lec5.Deployment where

import Control.Concurrent.Async

import ATMC.Lec5.History
import ATMC.Lec5.Agenda
import ATMC.Lec5.EventQueue
import ATMC.Lec5.Configuration
import ATMC.Lec5.Time
import ATMC.Lec5.TimerWheel
import ATMC.Lec5.Network

------------------------------------------------------------------------

data DeploymentMode = Production | Simulation Agenda History

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
    return Deployment
      { dMode          = mode
      , dConfiguration = config
      , dClock         = clock
      , dEventQueue    = eventQueue
      , dNetwork       = network
      , dTimerWheel    = timerWheel
      , dPids          = Pids []
      , dAppendHistory = \_ -> return ()
      }
  Simulation agenda history -> do
    clock      <- fakeClockEpoch
    eventQueue <- fakeEventQueue agenda clock
    network    <- fakeNetwork eventQueue clock
    timerWheel <- newTimerWheel
    return Deployment
      { dMode          = mode
      , dConfiguration = config
      , dClock         = clock
      , dEventQueue    = eventQueue
      , dNetwork       = network
      , dTimerWheel    = timerWheel
      , dPids          = Pids []
      , dAppendHistory = appendHistory history
      }
