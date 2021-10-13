module StuntDouble.Codec where

import Data.ByteString.Lazy (ByteString)

import StuntDouble.Envelope

------------------------------------------------------------------------

data Encode = Encode
  { encodeAddress       :: String
  , encodeCorrelationId :: Int
  , encodePayload       :: ByteString
  }

data Codec = Codec
  { codecEncode :: Envelope -> Encode
  , codecDecode :: ByteString -> Either String Envelope
  }

fakeCodec :: Codec
fakeCodec = Codec undefined undefined
