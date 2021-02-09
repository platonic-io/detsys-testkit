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

exTraces :: [Trace]
exTraces = [ [Event "A" "B", Event "A" "C"]
           , [Event "A" "B", Event "A" "R", Event "R" "S1", Event "R" "S2"]
           ]

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
  | Var String
  deriving Show

intersections :: (Foldable f, Ord a) => f (Set a) -> Set a
intersections = foldl1 Set.intersection

ldfi :: [Trace] -> Formula
ldfi ts =
  let
    ns = map nodes ts
  in
    And (map Var (Set.toList (intersections ns)))

main :: IO ()
main = return ()
