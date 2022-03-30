{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Dumblog.Common.Types where

import Data.Binary (Binary)

newtype SeqNum = SeqNum { unSeqNum :: Int }
  deriving newtype (Show, Binary)
