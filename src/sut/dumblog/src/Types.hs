module Types where

import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as LBS

data Command
  = Write ByteString
  | Read Int

type Response = LBS.ByteString
