module StateMachine where

import Data.ByteString (ByteString)

import Types

-- This is the main state of Dumblog, which is the result of applying all commands in the log
data InMemoryDumblog = InMemoryDumblog
  { theLog :: [ByteString] -- not very memory efficient, but not the point
  }

initState :: InMemoryDumblog
initState = InMemoryDumblog []

runCommand :: InMemoryDumblog -> Command -> IO (InMemoryDumblog, Response)
runCommand s _ = pure (s, error "Not implemented")
