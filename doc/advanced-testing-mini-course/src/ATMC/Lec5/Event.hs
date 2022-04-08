module ATMC.Lec5.Event where

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
