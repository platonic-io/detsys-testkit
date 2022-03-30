module Dumblog.SQLite.Command where

import Data.Int (Int64)
import Control.Concurrent.MVar
import Data.ByteString.Lazy.Char8

------------------------------------------------------------------------

data Command
  = Write ByteString Int64 (MVar Int)
  | Read Int Int64 (MVar (Maybe ByteString))

commandArrivalTime :: Command -> Int64
commandArrivalTime (Write _bs arrivalTime _response) = arrivalTime
commandArrivalTime (Read _ix arrivalTime _response)  = arrivalTime
