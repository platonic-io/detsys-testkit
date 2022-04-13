module ATMC.Lec5.EventQueue where

import Control.Concurrent.STM
import Control.Concurrent.STM.TQueue

import ATMC.Lec5.Event
import ATMC.Lec5.Options

------------------------------------------------------------------------

data EventQueue = EventQueue
  { eqEnqueue :: Event -> IO ()
  , eqDequeue :: IO Event
  }

realEventQueue :: IO EventQueue
realEventQueue = do
  q <- newTQueueIO
  return EventQueue
    { eqEnqueue = atomically . writeTQueue q
    , eqDequeue = atomically (readTQueue q)
    }

fakeEventQueue :: IO EventQueue
fakeEventQueue = undefined

newEventQueue :: DeploymentMode -> IO EventQueue
newEventQueue Production          = realEventQueue
newEventQueue (Simulation agenda) = fakeEventQueue
