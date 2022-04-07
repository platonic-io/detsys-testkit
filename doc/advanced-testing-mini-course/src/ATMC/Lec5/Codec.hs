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

prepareInput :: Codec req msg resp -> RawInput -> Maybe (Input req msg)
prepareInput codec (RawInput (ClientRequest at from to bs)) = do
  req <- cDecodeRequest codec bs
  return (ClientRequest at from to req)
