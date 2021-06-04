{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module StuntDouble.Envelope where

import Control.Concurrent.STM
import Control.Concurrent.Async

import StuntDouble.Actor.State
import StuntDouble.Message
import StuntDouble.Reference

------------------------------------------------------------------------

newtype CorrelationId = CorrelationId Int
  deriving (Eq, Ord, Show, Read, Num, Enum)

getCorrelationId :: CorrelationId -> Int
getCorrelationId (CorrelationId i) = i

data EnvelopeKind = RequestKind | ResponseKind
  deriving (Eq, Show, Read)

data Envelope = Envelope
  { envelopeKind          :: EnvelopeKind
  , envelopeSender        :: RemoteRef
  , envelopeMessage       :: Message
  , envelopeReceiver      :: RemoteRef
  , envelopeCorrelationId :: CorrelationId
  }
  deriving (Eq, Show, Read)

replyEnvelope :: Envelope -> Message -> Envelope
replyEnvelope e msg
  | envelopeKind e == RequestKind =
    e { envelopeKind = ResponseKind
      , envelopeSender   = envelopeReceiver e
      , envelopeMessage  = msg
      , envelopeReceiver = envelopeSender e
      }
  | otherwise = error "reply: impossilbe: can't reply to a response..."
