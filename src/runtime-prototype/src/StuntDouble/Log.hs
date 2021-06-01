module StuntDouble.Log where

------------------------------------------------------------------------

newtype Log = Log [LogEntry]

data LogEntry
  = LogEntry

emptyLog :: Log
emptyLog = Log []
