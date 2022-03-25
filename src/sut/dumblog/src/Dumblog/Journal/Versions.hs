module Dumblog.Journal.Versions where

import Data.Int(Int64)

import Dumblog.Journal.Logger (Logger)
import Dumblog.Journal.Types (Input, Output)
import Dumblog.Journal.StateMachine(InMemoryDumblog)
import qualified Dumblog.Journal.StateMachine as SM

dUMBLOG_CURRENT_VERSION :: Int64
dUMBLOG_CURRENT_VERSION = 1 -- 1 has bug, 2 fixes it

runCommand :: Int64 -> Logger -> InMemoryDumblog -> Input -> IO (InMemoryDumblog, Output)
runCommand 1 = SM.runCommand True
-- this line will be added in demo!
-- runCommand 2 = SM.runCommand False
runCommand v = \_ _ cmd ->
  error ("Don't know how to run the command: " <> show cmd <> " in version: " <> show v)
