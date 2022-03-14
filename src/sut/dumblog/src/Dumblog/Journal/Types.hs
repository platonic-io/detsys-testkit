{-# LANGUAGE DeriveGeneric #-}

module Dumblog.Journal.Types where

import Data.Binary (Binary)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as LBS
import GHC.Generics (Generic)

------------------------------------------------------------------------

data Command
  = Write ByteString
  | Read Int
  deriving (Generic, Show)

instance Binary Command

type Response = LBS.ByteString
