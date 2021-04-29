module StuntDouble.EventLoop.Event where

import Control.Concurrent.STM
import Control.Concurrent.Async

import StuntDouble.Actor
import StuntDouble.Message
import StuntDouble.Reference

------------------------------------------------------------------------

data Event
  = Command  Command
  | Response Response
  | Receive  Receive

eventName :: Event -> String
eventName (Command cmd) = "Command/" ++ commandName cmd

data Command
  = Spawn (Message -> Actor) (TMVar LocalRef)
  | Invoke LocalRef Message  (TMVar Message)
  | Send RemoteRef Message
  | Quit

commandName :: Command -> String
commandName Spawn {} = "Spawn"
commandName Invoke {} = "Invoke"
commandName Send {} = "Send"
commandName Quit {} = "Quit"

data Response
  = IOReady (Async IOResult)
  -- Receive (Async Message) Message

data Receive
  = Request Envelope

data Envelope = Envelope
  { envelopeSender   :: RemoteRef
  , envelopeMessage  :: Message
  , envelopeReceiver :: RemoteRef
  }
  deriving (Eq, Show, Read)
