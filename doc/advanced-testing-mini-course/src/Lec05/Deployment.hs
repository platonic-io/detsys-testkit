module Lec05.Deployment where

import Control.Concurrent.Async

import Lec05.History
import Lec05.Agenda
import Lec05.ErrorReporter
import Lec05.EventQueue
import Lec05.Configuration
import Lec05.Random
import Lec05.Time
import Lec05.TimerWheel
import Lec05.Network

------------------------------------------------------------------------

data DeploymentMode = Production | Simulation Seed Agenda History (Maybe FailureSpec) Collector

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
  , dReportError   :: ErrorReporter
  }

newtype Pids = Pids { unPids :: [Async ()] }

data FailureSpec = FailureSpec
  { fsNetworkFailure :: NetworkFaults
  }

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
      , dReportError   = putStrLn
      }
  Simulation seed agenda history mf errorCollector -> do
    clock      <- fakeClockEpoch
    eventQueue <- fakeEventQueue agenda clock
    random     <- fakeRandom seed
    network    <- faultyNetwork eventQueue clock random config history (fmap fsNetworkFailure mf)
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
      , dAppendHistory = appendHistory history DidArrive
      , dReportError   = reportWithCollector errorCollector
      }
