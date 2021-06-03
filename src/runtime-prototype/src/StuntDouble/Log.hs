module StuntDouble.Log where

import StuntDouble.Message
import StuntDouble.Reference
import StuntDouble.Actor.State

------------------------------------------------------------------------

newtype Log = Log [LogEntry]

data LogEntry
  = Spawned LocalRef State
  | Turn TurnData
  | ClientRequest
  | ClientResponse

data TurnData = TurnData
  { tdActor         :: LocalRef
  , tdBeforeState   :: State
  , tdMessage       :: Message
  , tdActions       :: [ActionLogEntry]
  , tdLogs          :: [LogLines]
  , tdAfterState    :: State
  , tdLogicalTime   :: Int
  , tdSimulatedTime :: Int -- XXX: UTCTime
  , tdReply         :: Message
  }

data ActionLogEntry = XXX
data LogLines = YYY

emptyLog :: Log
emptyLog = Log []
