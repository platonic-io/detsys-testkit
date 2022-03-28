{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}

module Dumblog.Journal.Types where

import Data.Binary (Binary)
import Data.ByteString.Lazy (ByteString)
import GHC.Generics (Generic)

------------------------------------------------------------------------

class CommandName c where
  commandName :: c -> String

data Input
  = ClientRequest ClientRequest
  | InternalMessageIn InternalMessage
  | AdminCommand AdminCommand
  deriving stock (Generic, Show)
  deriving anyclass Binary

instance CommandName Input where
  commandName c = case c of
    ClientRequest cr -> commandName cr
    InternalMessageIn im -> commandName im
    AdminCommand ac -> commandName ac

data ClientRequest
  = Write ByteString
  | Read Int
  deriving stock (Generic, Show)
  deriving anyclass Binary

instance CommandName ClientRequest where
  commandName (Read {})  = "read"
  commandName (Write {}) = "write"

data AdminCommand
  = Connect Int
  deriving stock (Generic, Show)
  deriving anyclass Binary

instance CommandName AdminCommand where
  commandName (Connect{}) = "connect"

data Output
  = ClientResponse ClientResponse
  | InternalMessageOut InternalMessage
  | AdminResponse
  deriving stock Show

instance CommandName Output where
  commandName c = case c of
    ClientResponse cr -> commandName cr
    InternalMessageOut im -> commandName im
    AdminResponse -> "admin" -- maybe not the nicest name

data ClientResponse
  = OK ByteString
  | NotFound
  | Error ByteString
  deriving stock Show

instance CommandName ClientResponse where
  commandName _ = "client" -- maybe not the nicest name

data InternalMessage
  = Backup Int ByteString
  | Ack Int
  deriving stock (Generic, Show)
  deriving anyclass Binary

instance CommandName InternalMessage where
  commandName (Backup {}) = "backup"
  commandName (Ack {}) = "ack"
