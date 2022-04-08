{-# LANGUAGE ExistentialQuantification #-}

module ATMC.Lec5.Codec where

import Data.Typeable
import Data.ByteString.Lazy (ByteString)

import ATMC.Lec5.StateMachine

------------------------------------------------------------------------

data Codec request message response = Codec
  { cDecodeRequest  :: ByteString -> Maybe request
  , cDecodeMessage  :: ByteString -> Maybe message
  , cEncodeResponse :: response -> ByteString
  , cEncodeMessage  :: message  -> ByteString
  }

data SomeCodecSM = forall state request message response. Typeable state =>
                   SomeCodecSM (Codec request message response)
                               (SM state request message response)

decodeInput :: Codec req msg resp -> Input ByteString ByteString -> Maybe (Input req msg)
decodeInput codec (ClientRequest at from bs) = do
  req <- cDecodeRequest codec bs
  return (ClientRequest at from req)

idCodec :: Codec ByteString ByteString ByteString
idCodec = Codec
  { cDecodeRequest  = Just
  , cDecodeMessage  = Just
  , cEncodeResponse = id
  , cEncodeMessage  = id
  }
