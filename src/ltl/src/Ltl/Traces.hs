module Ltl.Traces where

import Data.List.NonEmpty (NonEmpty)
import Data.Map (Map)
import Ltl.Json

type Node = String

data State = State (Map Node Json) deriving (Show)

data Event = Event Json
  deriving (Show)

data StateBehaviour = StateBehaviour
  { before :: State,
    worldTime :: Int,
    action :: Event,
    after :: State
  }
  deriving (Show)

type Trace = NonEmpty StateBehaviour
