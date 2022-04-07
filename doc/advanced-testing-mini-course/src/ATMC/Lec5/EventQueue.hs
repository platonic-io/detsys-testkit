module ATMC.Lec5.EventQueue where

import Control.Concurrent.STM
import Control.Concurrent.STM.TBQueue
import GHC.Natural

import ATMC.Lec5.Time
import ATMC.Lec5.StateMachine

------------------------------------------------------------------------

data Event
  = NetworkEvent RawInput
  | TimerEvent -- TimerEvent
  | CommandEvent CommandEvent

data CommandEvent = Exit

isExitCommand :: Event -> Bool
isExitCommand (CommandEvent Exit) = True
isExitCommand _otherwise          = False

eventTime :: Event -> Time
eventTime (NetworkEvent rawInput) = rawInputTime rawInput

newtype EventQueue = EventQueue (TBQueue Event)

qUEUE_SIZE :: Natural
qUEUE_SIZE = 4096

newEventQueue :: IO EventQueue
newEventQueue = do
  queue <- newTBQueueIO qUEUE_SIZE
  return (EventQueue queue)

enqueueEvent :: EventQueue -> Event -> IO ()
enqueueEvent (EventQueue queue) ev = atomically (writeTBQueue queue ev)

dequeueEvent :: EventQueue -> IO Event
dequeueEvent (EventQueue queue) = atomically (readTBQueue queue)
