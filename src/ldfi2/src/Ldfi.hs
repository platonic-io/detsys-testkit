module Ldfi where

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
exTraces =
  [ [Event "A" "B", Event "A" "C"]
  , [Event "A" "B", Event "A" "R", Event "R" "S1", Event "R" "S2"]
  ]

nodes :: Trace -> Set Node
nodes = foldMap (\e -> Set.singleton (from e) `mappend` Set.singleton (to e))

edges :: Trace -> Set Edge
edges = foldMap (\e -> Set.singleton (from e, to e))

infixr 3 :&&
infixr 2 :||

data Formula
  = Formula :&& Formula
  | Formula :|| Formula
  | And [Formula]
  | Or [Formula]
  | Neg Formula
  | TT
  | FF
  | Var String
  deriving (Eq, Show)

simplify :: Formula -> Formula
simplify (TT :&& r) = simplify r
simplify (l  :&& r) = simplify l :&& simplify r
simplify (FF :|| r) = simplify r
simplify (l  :|| r) = simplify l :|| simplify r
simplify (And [])   = TT
simplify (And [f])  = f
simplify (And fs)   = And (map simplify fs)

-- simplify (TT :&& r)     = simplify r
-- simplify (And xs :&& y) = And (map simplify xs ++ [simplify y])
-- simplify (x :&& And ys) = And (simplify x : map simplify ys)
-- simplify (FF :|| r)     = simplify r
-- simplify (l  :|| r)     = simplify l :|| simplify r
-- simplify (And fs)       = case filter (/= TT) . (>>= expandAnd) $ map simplify fs of
--   [] -> TT
--   [f] -> f
--   fs' -> And fs'
--   where
--     expandAnd (And xs) = xs
--     expandAnd (l :&& r) = [l, r]
--     expandAnd f = [f]
simplify f          = f

fixpoint :: Formula -> Formula
fixpoint f | simplify f == f = f
           | otherwise       = fixpoint (simplify f)

intersections :: (Foldable f, Ord a) => f (Set a) -> Set a
intersections = foldl1 Set.intersection

vars :: Set String -> Formula
vars = And . map Var . Set.toList

ldfi' :: [Trace] -> Formula
ldfi' ts =
  let
    ns  = map nodes ts
    is  = intersections ns
    c   = \i j -> (i `Set.intersection` j) Set.\\ is
    len = length ns `div` 2
  in
    vars is :&&
    And [ vars (c i j) :&& vars (i Set.\\ j) :|| vars (j Set.\\ i)
        | i <- take len ns
        , j <- drop len ns
        ]

ldfi :: [Trace] -> Formula
ldfi = fixpoint . ldfi'
