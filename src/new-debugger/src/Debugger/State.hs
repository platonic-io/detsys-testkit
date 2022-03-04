module Debugger.State where

data DebEvent = DebEvent
  { from :: String
  , to :: String
  , event :: String
  , receivedLogical :: Int
  -- , receivedSimulated :: Time
  , message :: String
  } deriving Show

data InstanceState = InstanceState
 { state :: String -- Should probably be per reactor
 , currentEvent :: DebEvent
 , seqDia :: String
 , logs :: [String]
 , sent :: [DebEvent]
 }
