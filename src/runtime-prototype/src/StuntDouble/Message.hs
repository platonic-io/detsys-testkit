{-# LANGUAGE DeriveGeneric #-}

module StuntDouble.Message where

import Data.Aeson (FromJSON, ToJSON, Value)
import GHC.Generics (Generic)

import StuntDouble.Reference

------------------------------------------------------------------------

type Tag = String

type Args = [SDatatype]

data SDatatype
  = SInt Int
  | SFloat Float
  | SString String
  -- | SBlob ByteString XXX: No To/FromJSON instances...
  | STimestamp Double
  | SList [SDatatype]
  | SValue Value
  deriving (Eq, Show, Read, Generic)

instance ToJSON SDatatype
instance FromJSON SDatatype

data Message
  = InternalMessage String -- XXX: remove unprimed variants...
  | InternalMessage' Tag Value
  | ClientRequest Tag ClientRef
  | ClientRequest' Tag Args ClientRef
  | ClientRequest'' Tag Args
  deriving (Eq, Show, Read, Generic)

instance ToJSON Message
instance FromJSON Message

getMessage :: Message -> String
getMessage (InternalMessage msg) = msg
getMessage (InternalMessage' msg _args) = msg
getMessage (ClientRequest msg _cid) = msg
getMessage (ClientRequest' msg _args _cid) = msg
getMessage (ClientRequest'' msg _args) = msg

getArgs :: Message -> Args
getArgs InternalMessage {}               = []
getArgs (InternalMessage' _msg args)     = [SValue args]
getArgs ClientRequest {}                 = []
getArgs (ClientRequest' _msg args _cref) = args
getArgs (ClientRequest'' _msg args)      = args
