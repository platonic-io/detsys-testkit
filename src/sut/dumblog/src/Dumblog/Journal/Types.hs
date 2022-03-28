{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}

module Dumblog.Journal.Types where

import Data.Binary (Binary)
import Data.ByteString.Lazy (ByteString)
import GHC.Generics (Generic)

------------------------------------------------------------------------

newtype SeqNum = SeqNum { unSeqNum :: Int }
  deriving newtype (Show, Binary)

data Input
  = ClientRequest ClientRequest SeqNum
  | InternalMessageIn InternalMessage
  | AdminCommand AdminCommand
  deriving stock (Generic, Show)
  deriving anyclass Binary

data ClientRequest
  = Write ByteString
  | Read Int
  deriving stock (Generic, Show)
  deriving anyclass Binary

data AdminCommand
  = Connect Int
  deriving stock (Generic, Show)
  deriving anyclass Binary

data Output
  = ClientResponse ClientResponse SeqNum
  | InternalMessageOut InternalMessage
  | AdminResponse
  deriving stock Show

data ClientResponse
  = OK ByteString
  | NotFound
  | Error ByteString
  deriving stock Show

data InternalMessage
  = Backup Int ByteString SeqNum
  | Ack Int SeqNum
  deriving stock (Generic, Show)
  deriving anyclass Binary
