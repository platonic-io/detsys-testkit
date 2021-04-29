{-# LANGUAGE GeneralizedNewtypeDeriving #-}

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
eventName (Command cmd)  = "Command/" ++ commandName cmd
eventName (Response resp) = "Response/" ++ responseName resp
eventName (Receive recv) = "Receive/" ++ receiveName recv

data Command
  = Spawn (Message -> Actor) (TMVar LocalRef)
  | Invoke LocalRef Message  (TMVar Message)
  | Send RemoteRef Message   (TMVar (Async Message))
  | Quit

commandName :: Command -> String
commandName Spawn  {} = "Spawn"
commandName Invoke {} = "Invoke"
commandName Send   {} = "Send"
commandName Quit   {} = "Quit"

data Response
  = IOReady (Async IOResult)
  | Reply (TMVar Message) Envelope

responseName :: Response -> String
responseName IOReady {} = "IOReady"
responseName Reply   {} = "Reply"

data Receive
  = Request Envelope

receiveName :: Receive -> String
receiveName Request {} = "Request"

newtype CorrelationId = CorrelationId Int
  deriving (Eq, Ord, Show, Read, Num, Enum)

data Envelope = Envelope
  { envelopeSender        :: RemoteRef
  , envelopeMessage       :: Message
  , envelopeReceiver      :: RemoteRef
  , envelopeCorrelationId :: CorrelationId
  }
  deriving (Eq, Show, Read)

reply :: Envelope -> Message -> Envelope
reply e reply =
  e { envelopeSender   = envelopeReceiver e
    , envelopeMessage  = reply
    , envelopeReceiver = envelopeSender e
    }
