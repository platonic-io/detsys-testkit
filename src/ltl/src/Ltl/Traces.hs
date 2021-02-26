module Ltl.Traces where

import Data.Map(Map)
import Data.List.NonEmpty (NonEmpty)

import Ltl.Json

type Node = String

data State = State (Map Node Json) deriving (Show)

data Event = Event -- we will extend this later
  deriving (Show)

data StateBehaviour = StateBehaviour
  { before :: State
  , action :: Event
  , after  :: State
  } deriving (Show)

type Trace = NonEmpty StateBehaviour
