module Lec05.EventQueue where

import Control.Concurrent.STM
       (atomically, newTQueueIO, readTQueue, writeTQueue)
import Data.IORef
import Data.List (sortOn)
import System.Timeout (timeout)

import Lec05.Agenda
import Lec05.ClientGenerator
import Lec05.Event
import Lec05.Time

------------------------------------------------------------------------

data DequeueTimeout
  = NoTimeout
  | Timeout Int        -- ^ Micro seconds.
            (IO Event) -- ^ Retry action.

data EventQueue = EventQueue
  { eqEnqueue :: Event -> IO ()
  , eqDequeue :: DequeueTimeout -> IO Event
  }

realEventQueue :: Clock -> IO EventQueue
realEventQueue _clock = do
  q <- newTQueueIO
  return EventQueue
    { eqEnqueue = atomically . writeTQueue q
    , eqDequeue = \dequeueTimeout -> case dequeueTimeout of
        NoTimeout -> atomically (readTQueue q)
        Timeout micros retry -> do
          mEvent <- timeout micros (atomically (readTQueue q))
          case mEvent of
            Nothing    -> retry
            Just event -> return event
    }

fakeEventQueue :: Agenda -> Clock -> ClientGenerator -> IO EventQueue
fakeEventQueue a0 clock clientGenerator = do
  agenda <- newIORef a0
  return EventQueue
    { eqEnqueue = enqueue agenda
    , eqDequeue = dequeue agenda
    }
  where
    enqueue :: IORef Agenda -> Event -> IO ()
    enqueue agenda event = modifyIORef' agenda (push (getEventTime event, event))

    dequeue :: IORef Agenda -> DequeueTimeout -> IO Event
    dequeue agenda dequeueTimeout = do
      a <- readIORef agenda
      now <- cGetCurrentTime clock
      ncr <- cgNextClientRequest clientGenerator
      let allEvents = foldr (.) id
            [ case pop a of
                Nothing -> id
                Just ((time, event), a') -> push (time, writeIORef agenda a' >> return event)
            , case dequeueTimeout of
                NoTimeout -> id
                Timeout micros retry -> push (addTimeMicros micros now, retry)
            , case ncr of
                CurrentlyNoRequests -> id
                Now e -> push (now, return e)
                Later t e -> push (t, return e)
            ]
            emptyAgenda
      case pop allEvents of
        Nothing -> return (CommandEventE Exit)
        Just ((_time, actionToGenerateEvent),_) ->
          -- set time?
          actionToGenerateEvent
