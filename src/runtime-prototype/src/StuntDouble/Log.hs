module StuntDouble.Log where

import Control.Exception

import StuntDouble.Message
import StuntDouble.Reference

------------------------------------------------------------------------

newtype Log = Log [LogEntry]

data LogEntry
  = Spawned LocalRef
  | Turn TurnData
  | ClientRequestEntry
  | ClientResponseEntry
  | ErrorLogEntry SomeException

data TurnData = TurnData
  { tdActor         :: LocalRef
  , tdBeforeState   :: String -- XXX: State
  , tdMessage       :: Message
  , tdActions       :: [ActionLogEntry]
  , tdLogs          :: [LogLines]
  , tdAfterState    :: String -- XXX: State
  , tdLogicalTime   :: Int
  , tdSimulatedTime :: Int -- XXX: UTCTime
  , tdReply         :: Message
  }

data ActionLogEntry = XXX
data LogLines = YYY

emptyLog :: Log
emptyLog = Log []

  {-

type EventLog = [LogEntry]

data LogEntry
  = LogInvoke RemoteRef LocalRef Message Message EventLoopName
  | LogSendStart RemoteRef RemoteRef Message CorrelationId EventLoopName
  | LogSendFinish CorrelationId Message EventLoopName
  | LogRequest RemoteRef RemoteRef Message Message EventLoopName
  | LogReceive RemoteRef RemoteRef Message CorrelationId EventLoopName
  | LogRequestStart RemoteRef RemoteRef Message CorrelationId EventLoopName
  | LogRequestFinish CorrelationId Message EventLoopName
  | LogComment String EventLoopName
  | LogAsyncIOFinish CorrelationId IOResult EventLoopName
  deriving (Eq, Show)

isComment :: LogEntry -> Bool
isComment LogComment {} = True
isComment _otherwise    = False
-}
