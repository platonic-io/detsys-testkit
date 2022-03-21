{-# LANGUAGE DeriveGeneric #-}

module Dumblog.Journal.Types where

import Data.Binary (Binary)
import Data.ByteString.Lazy (ByteString)
import GHC.Generics (Generic)

------------------------------------------------------------------------

data Command
  = Write ByteString
  | Read Int
  deriving (Generic, Show)

instance Binary Command

data Response
  = OK ByteString
  | NotFound
  | Error ByteString
  deriving (Show)
