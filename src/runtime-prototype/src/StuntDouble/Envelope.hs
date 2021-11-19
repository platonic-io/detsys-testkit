{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveGeneric #-}

module StuntDouble.Envelope where

import Data.ByteString (ByteString)
import Data.Aeson (FromJSON, ToJSON, toEncoding, parseJSON)
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

type Envelope = Envelope' Message

data Envelope' msg = Envelope
  { envelopeKind          :: EnvelopeKind
  , envelopeSender        :: RemoteRef
  , envelopeMessage       :: msg
  , envelopeReceiver      :: RemoteRef
  , envelopeCorrelationId :: CorrelationId
  -- | NOTE: We don't need to send the `NodeName` part of `LogicalTime`, only
  -- the integer part over the wire...
  , envelopeLogicalTime   :: LogicalTimeInt
  }
  deriving (Eq, Show, Read, Generic)

-- XXX: We probably want to remove this instance and have a separate envelope
-- codec in the future, which should probably come with a companion message
-- codec.
instance FromJSON msg => FromJSON (Envelope' msg)

replyEnvelope :: Envelope' msg -> Message -> Envelope
replyEnvelope e msg
  | envelopeKind e == RequestKind =
    e { envelopeKind = ResponseKind
      , envelopeSender   = envelopeReceiver e
      , envelopeMessage  = msg
      , envelopeReceiver = envelopeSender e
      }
  | otherwise = error "reply: impossilbe: can't reply to a response..."
