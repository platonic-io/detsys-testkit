{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}

module Dumblog.Journal.Types where

import Data.Binary (Binary)
import Data.ByteString.Lazy (ByteString)
import GHC.Generics (Generic)

------------------------------------------------------------------------

data Input
  = ClientRequest ClientRequest
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
  = ClientResponse ClientResponse
  | InternalMessageOut InternalMessage
  | AdminResponse
  deriving stock Show

data ClientResponse
  = OK ByteString
  | NotFound
  | Error ByteString
  deriving stock Show

data InternalMessage
  = Backup Int ByteString
  | Ack Int
  deriving stock (Generic, Show)
  deriving anyclass Binary
