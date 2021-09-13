module StuntDouble.Codec where

import Data.ByteString.Lazy (ByteString)

import StuntDouble.Envelope

------------------------------------------------------------------------

data Encode = Encode
  { encodeAddress :: String
  , encodePayload :: ByteString
  }

data Codec = Codec
  { codecEncode :: Envelope -> Encode
  , codecDecode :: ByteString -> Either String Envelope
  }
