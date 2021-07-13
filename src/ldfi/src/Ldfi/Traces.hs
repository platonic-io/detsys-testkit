{-# LANGUAGE DeriveGeneric #-}

module Ldfi.Traces where

import Data.Hashable (Hashable)
import Data.Set (Set)
import qualified Data.Set as Set
import GHC.Generics (Generic)
import GHC.Natural

------------------------------------------------------------------------

-- * Traces

type Trace = [Event]

data Event = Event
  { from :: Node,
    sent :: Time,
    to :: Node,
    recv :: Time
  }
  deriving (Eq, Ord, Read, Show, Generic)

instance Hashable Event

type Node = String

type Edge = (Node, Node)

type Time = Natural

nodes :: Trace -> Set Node
nodes = foldMap (\e -> Set.singleton (from e) `mappend` Set.singleton (to e))

edges :: Trace -> Set Edge
edges = foldMap (\e -> Set.singleton (from e, to e))
