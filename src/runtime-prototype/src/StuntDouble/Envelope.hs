{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveGeneric #-}

module StuntDouble.Envelope where

import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)
import Control.Concurrent.STM
import Control.Concurrent.Async

import StuntDouble.Message
import StuntDouble.Reference
import StuntDouble.LogicalTime

------------------------------------------------------------------------

newtype CorrelationId = CorrelationId Int
  deriving (Eq, Ord, Show, Read, Num, Enum, Generic)

instance ToJSON CorrelationId
instance FromJSON CorrelationId

getCorrelationId :: CorrelationId -> Int
getCorrelationId (CorrelationId i) = i

data EnvelopeKind = RequestKind | ResponseKind
  deriving (Eq, Show, Read, Generic)

instance ToJSON EnvelopeKind
instance FromJSON EnvelopeKind

data Envelope = Envelope
  { envelopeKind          :: EnvelopeKind
  , envelopeSender        :: RemoteRef
  , envelopeMessage       :: Message
  , envelopeReceiver      :: RemoteRef
  , envelopeCorrelationId :: CorrelationId
  , envelopeLogicalTime   :: LogicalTime -- XXX: we don't need to send
                                         -- the NodeName part, only the
                                         -- integer part over the wire...
  }
  deriving (Generic, Eq, Show, Read)

instance ToJSON Envelope
instance FromJSON Envelope

replyEnvelope :: Envelope -> Message -> Envelope
replyEnvelope e msg
  | envelopeKind e == RequestKind =
    e { envelopeKind = ResponseKind
      , envelopeSender   = envelopeReceiver e
      , envelopeMessage  = msg
      , envelopeReceiver = envelopeSender e
      }
  | otherwise = error "reply: impossilbe: can't reply to a response..."
