{-# LANGUAGE ExistentialQuantification #-}

module ATMC.Lec5.Codec where

import Data.Typeable
import Data.ByteString.Lazy (ByteString)

import ATMC.Lec5.StateMachine

------------------------------------------------------------------------

data Codec request message response = Codec
  { cDecode :: ByteString -> Maybe (Input request message)
  , cEncode :: Either response message -> ByteString
  }

data SomeCodecSM = forall state request message response. Typeable state =>
                   SomeCodecSM (Codec request message response)
                               (SM state request message response)
