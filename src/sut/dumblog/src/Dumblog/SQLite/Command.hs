module Dumblog.SQLite.Command where

import Control.Concurrent.MVar
import Data.ByteString.Lazy.Char8

------------------------------------------------------------------------

data Command
  = Write ByteString (MVar Int)
  | Read Int (MVar ByteString)
