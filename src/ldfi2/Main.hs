module Main where

import Data.Set (Set)
import qualified Data.Set as Set

type Node = String

type Edge = (Node, Node)

data Event = Event
  { from :: Node
  , to   :: Node
  }
  deriving (Eq, Ord, Show)

type Trace = [Event]

nodes :: Trace -> Set Node
nodes = foldMap (\e -> Set.singleton (from e) `mappend` Set.singleton (to e))

edges :: Trace -> Set Edge
edges = foldMap (\e -> Set.singleton (from e, to e))

data Formula
  = Formula :&& Formula
  | Formula :|| Formula
  | And [Formula]
  | Or [Formula]
  | Neg Formula
  | TT
  | FF
  deriving Show

ldfi :: [Trace] -> Formula
ldfi [] = FF

main :: IO ()
main = return ()
