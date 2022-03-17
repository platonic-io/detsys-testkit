module Dumblog.Journal.Versions.Codec where

import Data.Int(Int64)

import Dumblog.Journal.Logger (Logger)
import Dumblog.Journal.Types (Command, Response)
import Dumblog.Journal.StateMachine(InMemoryDumblog)
import qualified Dumblog.Journal.StateMachine as SM

runCommand :: Int64 -> Logger -> InMemoryDumblog -> Command -> IO (InMemoryDumblog, Response)
runCommand 1 = SM.runCommand True
-- this line will be added in demo!
-- runCommand 2 = SM.runCommand False
runCommand v = \_ _ cmd -> error ("Don't know how to run the command: " <> show cmd <> " in version: " <> show v)
