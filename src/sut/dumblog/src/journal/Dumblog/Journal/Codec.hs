{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}

module Dumblog.Journal.Codec where

import Data.Binary (Binary)
import Data.Int (Int64)
import GHC.Generics (Generic)

------------------------------------------------------------------------

data Envelope a = Envelope
  { eContent :: !a
  , eVersion :: !Int64
  , eArrival :: !Int64 -- Nano seconds since epoch.
  } deriving stock (Functor, Generic)

instance Binary a => Binary (Envelope a)
