{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveGeneric #-}

module StuntDouble.Envelope where

import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)
import Control.Concurrent.STM
import Control.Concurrent.Async

import StuntDouble.Actor.State
import StuntDouble.Message
import StuntDouble.Reference

------------------------------------------------------------------------

newtype CorrelationId = CorrelationId Int
  deriving (Eq, Ord, Show, Read, Num, Enum, Generic)

instance ToJSON CorrelationId where
instance FromJSON CorrelationId where

getCorrelationId :: CorrelationId -> Int
getCorrelationId (CorrelationId i) = i

data EnvelopeKind = RequestKind | ResponseKind
  deriving (Eq, Show, Read, Generic)

instance ToJSON EnvelopeKind where
instance FromJSON EnvelopeKind where

data Envelope = Envelope
  { envelopeKind          :: EnvelopeKind
  , envelopeSender        :: RemoteRef
  , envelopeMessage       :: Message
  , envelopeReceiver      :: RemoteRef
  , envelopeCorrelationId :: CorrelationId
  }
  deriving (Generic, Eq, Show, Read)

instance ToJSON Envelope where
instance FromJSON Envelope where

replyEnvelope :: Envelope -> Message -> Envelope
replyEnvelope e msg
  | envelopeKind e == RequestKind =
    e { envelopeKind = ResponseKind
      , envelopeSender   = envelopeReceiver e
      , envelopeMessage  = msg
      , envelopeReceiver = envelopeSender e
      }
  | otherwise = error "reply: impossilbe: can't reply to a response..."
