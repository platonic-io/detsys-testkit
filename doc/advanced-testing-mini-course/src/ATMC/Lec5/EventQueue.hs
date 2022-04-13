module ATMC.Lec5.EventQueue where

import Control.Concurrent.STM
import Control.Concurrent.STM.TQueue
import Data.IORef

import ATMC.Lec5.Agenda
import ATMC.Lec5.Event
import ATMC.Lec5.Time
import ATMC.Lec5.Options

------------------------------------------------------------------------

data EventQueue = EventQueue
  { eqEnqueue :: Event -> IO ()
  , eqDequeue :: IO Event
  }

realEventQueue :: Clock -> IO EventQueue
realEventQueue _clock = do
  q <- newTQueueIO
  return EventQueue
    { eqEnqueue = atomically . writeTQueue q
    , eqDequeue = atomically (readTQueue q)
    }

fakeEventQueue :: Agenda -> Clock -> IO EventQueue
fakeEventQueue a clock = do
  agenda <- newIORef a
  return EventQueue
    { eqEnqueue = enqueue agenda
    , eqDequeue = dequeue agenda
    }
  where
    enqueue :: IORef Agenda -> Event -> IO ()
    enqueue agenda event = do
      now <- cGetCurrentTime clock
      -- XXX: need seed to generate random arrival time
      let arrivalTime = addTime 1 now
      modifyIORef' agenda (push (arrivalTime, setEventTime arrivalTime event))

    dequeue :: IORef Agenda -> IO Event
    dequeue agenda = do
      a <- readIORef agenda
      case pop a of
        Nothing -> return (CommandEventE Exit)
        Just ((_time, event), a') -> do
          writeIORef agenda a'
          return event

newEventQueue :: DeploymentMode -> Clock -> IO EventQueue
newEventQueue Production          = realEventQueue
newEventQueue (Simulation agenda) = fakeEventQueue agenda
