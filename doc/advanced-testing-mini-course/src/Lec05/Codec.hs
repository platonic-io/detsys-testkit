module Lec05.Codec where

import Data.ByteString.Lazy (ByteString)
import Data.Text.Lazy (pack, unpack)
import Data.Text.Lazy.Encoding (decodeUtf8, encodeUtf8)
import Text.Read (readMaybe)

import Lec05.StateMachine

------------------------------------------------------------------------

data Codec request message response = Codec
  { cDecodeRequest  :: ByteString -> Maybe request
  , cDecodeMessage  :: ByteString -> Maybe message
  , cEncodeResponse :: response -> ByteString
  , cEncodeMessage  :: message  -> ByteString
  }

decodeInput :: Codec req msg resp -> Input ByteString ByteString -> Maybe (Input req msg)
decodeInput codec (ClientRequest at from bs) = do
  req <- cDecodeRequest codec bs
  return (ClientRequest at from req)
decodeInput codec (InternalMessage at from bs) = do
  msg <- cDecodeMessage codec bs
  return (InternalMessage at from msg)

idCodec :: Codec ByteString ByteString ByteString
idCodec = Codec
  { cDecodeRequest  = Just
  , cDecodeMessage  = Just
  , cEncodeResponse = id
  , cEncodeMessage  = id
  }

encShow :: Show a => a -> ByteString
encShow = encodeUtf8 . pack . show

decRead :: Read a => ByteString -> Maybe a
decRead = readMaybe . unpack . decodeUtf8

-- not performant but good for quickly getting something up and running
showReadCodec
  :: (Read request)
  => (Read message, Show message)
  => (Show response)
  => Codec request message response
showReadCodec = Codec
  { cDecodeRequest  = decRead
  , cDecodeMessage  = decRead
  , cEncodeResponse = encShow
  , cEncodeMessage  = encShow
  }
