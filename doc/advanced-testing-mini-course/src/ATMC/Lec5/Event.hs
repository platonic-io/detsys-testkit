module ATMC.Lec5.Event where

import Data.ByteString.Lazy (ByteString)

import ATMC.Lec5.Time
import ATMC.Lec5.StateMachine

------------------------------------------------------------------------

data Event
  = NetworkEvent RawInput
  | TimerEvent -- TimerEvent
  | CommandEvent CommandEvent

data RawInput = RawInput NodeId (Input ByteString ByteString)
  deriving Show

inputTime :: Input request message -> Time
inputTime (ClientRequest   time _cid _req) = time
inputTime (InternalMessage time _nid _msg) = time

rawInputTime :: RawInput -> Time
rawInputTime (RawInput _to input) = inputTime input

data CommandEvent = Exit

isExitCommand :: Event -> Bool
isExitCommand (CommandEvent Exit) = True
isExitCommand _otherwise          = False

eventTime :: Event -> Time
eventTime (NetworkEvent rawInput) = rawInputTime rawInput
