{-# LANGUAGE DeriveGeneric, DeriveFunctor#-}
module Codec where

import Data.Binary (Binary)
import qualified Data.Binary as Binary
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as LBS

import GHC.Generics(Generic)

data Envelope a = Envelope
  { eLength :: Int
  , eContent :: a
  } deriving (Functor, Generic)

instance Binary a => Binary (Envelope a) where

encode :: Binary a => Envelope a -> ByteString
encode e = LBS.toStrict (Binary.encode e)

-- this is guaranteed not to copy the bytestring
-- but we should probably allow this to fail
decode :: Binary a => ByteString -> Envelope a
decode bs = Binary.decode $ LBS.fromStrict bs
