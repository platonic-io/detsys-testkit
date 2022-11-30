{-# language RankNTypes #-}
module Scheduler.Time where

type Timestamp = Int
type Duration = Int -- Use better types

-- specify ns?
timeAdd :: Timestamp -> Duration -> Timestamp
timeAdd = (+)

after :: Timestamp -> Timestamp -> Bool
after t1 t2 = t1 >= t2

newtype LogicalTime = LogicalTime {theLogicalTime :: Int}

inc :: LogicalTime -> LogicalTime
inc (LogicalTime l) = LogicalTime $ 1 + l

data BumpLogical = BumpLogical | KeepLogical

data Capability m = Capability
  { currentSimulatedClock :: m Timestamp
  , currentLogicalClock :: m LogicalTime
  , advanceTime :: Timestamp -> BumpLogical -> m ()
  -- setters for these
  }


data State = State
  { simulatedClock :: Timestamp
  , logicalClock :: LogicalTime
  }

empty :: State
empty = State
  { simulatedClock = 0
  , logicalClock = LogicalTime 0
  }

fromModify :: Monad m => (forall b. (State -> (State, b)) -> m b) -> Capability m
fromModify modify = Capability
  { currentSimulatedClock = modify $ \s -> (s, simulatedClock s)
  , currentLogicalClock = modify $ \s -> (s, logicalClock s)
  , advanceTime = \t bl -> do
      modify $ \s -> (case bl of
        BumpLogical -> s { simulatedClock = t, logicalClock = inc $ logicalClock s}
        KeepLogical -> s { simulatedClock = t}
          , ())
  }
