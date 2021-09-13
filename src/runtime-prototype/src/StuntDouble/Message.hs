{-# LANGUAGE DeriveGeneric #-}

module StuntDouble.Message where

import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)
import Data.HashMap.Strict (HashMap)

import StuntDouble.Reference

------------------------------------------------------------------------

type Verb = String

type Args = [SDatatype]

data SDatatype
  = SInt Int
  | SFloat Float
  | SString String
  -- | SBlob ByteString XXX: No To/FromJSON instances...
  | STimestamp Double
  | SList [SDatatype]
  deriving (Eq, Show, Read, Generic)

instance ToJSON SDatatype
instance FromJSON SDatatype

data Message
  = InternalMessage String -- XXX: remove unprimed variants...
  | InternalMessage' Verb Args
  | ClientRequest String ClientRef
  | ClientRequest' Verb Args ClientRef
  deriving (Eq, Show, Read, Generic)

instance ToJSON Message
instance FromJSON Message

getMessage :: Message -> String
getMessage (InternalMessage msg) = msg
getMessage (InternalMessage' msg _args) = msg
getMessage (ClientRequest msg _cid) = msg
getMessage (ClientRequest' msg _args _cid) = msg

getArgs :: Message -> Args
getArgs InternalMessage {}               = []
getArgs (InternalMessage' _msg args)     = args
getArgs ClientRequest {}                 = []
getArgs (ClientRequest' _msg args _cref) = args
