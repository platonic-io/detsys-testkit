module Lec05.EventQueue where

import Control.Concurrent.STM
import Data.IORef

import Lec05.Agenda
import Lec05.Event
import Lec05.Time

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
fakeEventQueue a0 _clock = do
  agenda <- newIORef a0
  return EventQueue
    { eqEnqueue = enqueue agenda
    , eqDequeue = dequeue agenda
    }
  where
    enqueue :: IORef Agenda -> Event -> IO ()
    enqueue agenda event = modifyIORef' agenda (push (getEventTime event, event))

    dequeue :: IORef Agenda -> IO Event
    dequeue agenda = do
      a <- readIORef agenda
      case pop a of
        Nothing -> return (CommandEventE Exit)
        Just ((_time, event), a') -> do
          writeIORef agenda a'
          return event
