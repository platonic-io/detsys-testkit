module StuntDouble.Message where

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
  deriving (Eq, Show, Read)

data Message
  = InternalMessage String -- XXX: remove unprimed variants...
  | InternalMessage' Verb Args
  | ClientRequest String ClientRef
  | ClientRequest' Verb Args ClientRef
  deriving (Eq, Show, Read)

getMessage :: Message -> String
getMessage (InternalMessage msg) = msg
getMessage (InternalMessage' msg _args) = msg
getMessage (ClientRequest msg _cid) = msg
getMessage (ClientRequest' msg _args _cid) = msg
