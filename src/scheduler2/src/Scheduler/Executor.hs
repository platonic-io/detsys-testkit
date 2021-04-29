module Scheduler.Executor where

import Scheduler.Json (Json)
import Scheduler.Time (Timestamp, Duration)

-- Messages <- executor
data UEKind = Ok | Message

data UnScheduledEvent = UnScheduledEvent
  { ueTo :: [String],
    ueFrom :: String,
    ueKind :: UEKind,
    ueEvent ::  String,
    ueArgs :: Json
  }

data TKind = Timer

data TimerEvent = TimerEvent
  { teKind :: TKind,
    teArgs :: Json,
    teFrom :: String,
    teDuration :: Duration
  }

data OutEvent
  = OEUnscheduledEvent UnScheduledEvent
  | OETimer TimerEvent
type Events = [OutEvent]

-- Message -> executor
data ClientRequest = ClientRequest
  { crId :: Int
  , crRequest :: Json
  }

-- I think we don't care
data InEvent
  = IEClientRequest ClientRequest
  | IEInternalMessage Json


data MetaInfo = MetaInfo
  { testId :: Int -- "test-id"
  , runId :: Int -- "run-id"
  , logicalTime :: Int -- "logical-time"
  }
data KindEvent = KETimer | KEInternalMessage | KEClient

data ScheduledEvent
  = ScheduledEvent
  { at :: Timestamp
  , kind :: KindEvent
  , from :: String
  , to :: String
  , event :: String
  , args :: Json -- InEvent?
  , meta :: MetaInfo
  }

-- Capability
data Capability ref m = Capability
    --tick :: ref -> Timestamp -> m Events
  { execute :: ref -> ScheduledEvent -> m Events
  , timer :: ref -> ScheduledEvent -> m Events
  }
