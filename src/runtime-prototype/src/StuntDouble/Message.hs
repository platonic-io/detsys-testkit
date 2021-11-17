{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module StuntDouble.Message where

import Data.Aeson
       ( FromJSON
       , ToJSON
       , Value
       , defaultOptions
       , genericParseJSON
       , genericToJSON
       , object
       , parseJSON
       , toJSON
       , withObject
       , (.:)
       , (.=)
       )
import GHC.Generics (Generic)
import Control.Applicative

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

instance ToJSON Message where
  toJSON (InternalMessage' t v) = object ["kind" .= t, "message" .= v]
  toJSON msg = genericToJSON defaultOptions msg

instance FromJSON Message where
  parseJSON obj =
    withObject "InternalMessage'" (\v -> InternalMessage'
      <$> v .: "kind"
      <*> v .: "message") obj
    <|> genericParseJSON defaultOptions obj


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
