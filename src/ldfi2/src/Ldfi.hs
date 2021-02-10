{-# LANGUAGE DeriveFoldable #-}

module Ldfi where

import Data.Set (Set)
import qualified Data.Set as Set
import Numeric.Natural
import Z3.Monad

------------------------------------------------------------------------
-- * Traces

type Trace = [Event]

data Event = Event
  { from :: Node
  , to   :: Node
  , at   :: Time
  }
  deriving (Eq, Ord, Show)

type Node = String

type Edge = (Node, Node)

type Time = Natural

nodes :: Trace -> Set Node
nodes = foldMap (\e -> Set.singleton (from e) `mappend` Set.singleton (to e))

edges :: Trace -> Set Edge
edges = foldMap (\e -> Set.singleton (from e, to e))

------------------------------------------------------------------------
-- * Propositional logic formulae

infixr 3 :&&
infixr 2 :||

data FormulaF var
  = Formula :&& Formula
  | Formula :|| Formula
  | And [Formula]
  | Or [Formula]
  | Neg Formula
  | TT
  | FF
  | Var var
  deriving (Eq, Show, Foldable)

type Formula = FormulaF String

getVars :: Ord var => FormulaF var -> Set var
getVars = foldMap Set.singleton

simplify1 :: Formula -> Formula
simplify1 (TT :&& r)  = simplify1 r
simplify1 (l  :&& r)  = simplify1 l :&& simplify1 r
simplify1 (FF :|| r)  = simplify1 r
simplify1 (l  :|| TT) = simplify1 l
simplify1 (l  :|| r)  = simplify1 l :|| simplify1 r
simplify1 (And [])    = TT
simplify1 (And [f])   = f
simplify1 (And fs)    = And (map simplify1 fs)
simplify1 f           = f

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

fixpoint :: Formula -> Formula
fixpoint f | simplify1 f == f = f
           | otherwise        = fixpoint (simplify1 f)

vars :: Set String -> Formula
vars = And . map Var . Set.toList

------------------------------------------------------------------------
-- * Failure specification

data FailureSpec = FailureSpec
  { endOfFiniteFailures :: Time    -- ^ When finite failures, i.e. omissions,
                                   -- stop (a.k.a. EOF).
  , maxCrashes          :: Natural -- ^ The maximum amount of crashes allowed.
  , endOfTime           :: Time    -- ^ When the test stops (a.k.a. EOT).
  }

------------------------------------------------------------------------
-- * Lineage-driven fault injection

intersections :: (Foldable f, Ord a) => f (Set a) -> Set a
intersections = foldl1 Set.intersection

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

------------------------------------------------------------------------
-- * SAT formula

toSatAst :: MonadZ3 z3 => Formula -> z3 AST
toSatAst = undefined
