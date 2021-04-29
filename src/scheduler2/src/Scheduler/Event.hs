module Scheduler.Event where

import Scheduler.Json (Json)

data Event
  = Event
  { kind :: String
  , event :: String
  , args :: Json
  , to :: String
  , from :: String
  , sentAt :: Maybe Int
  }
  deriving (Eq, Ord) -- Ord to make determinstic

newtype ClientResponse = ClientResponse Event
