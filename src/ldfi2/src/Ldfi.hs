module Ldfi where

import Data.Set (Set)
import qualified Data.Set as Set

import Ldfi.FailureSpec
import Ldfi.Prop
import Ldfi.Traces

------------------------------------------------------------------------
-- * Lineage-driven fault injection

intersections :: (Foldable f, Ord a) => f (Set a) -> Set a
intersections = foldl1 Set.intersection

lineage :: [Trace] -> Formula
lineage ts =
  -- Or [ makeVars t | t <- map nodes ts]
  let
    ns  = map nodes ts
    is  = intersections ns
    c   = \i j -> (i `Set.intersection` j) Set.\\ is
    len = length ns `div` 2
  in
    makeVars is :&&
    And [ makeVars (c i j) :&& (makeVars (i Set.\\ j) :|| makeVars (j Set.\\ i))
        | i <- take len ns
        , j <- drop len ns
        ]

-- This probably needs the [Trace] ?
failureSpecConstraint :: FailureSpec -> Formula
failureSpecConstraint _ = TT

ldfi :: FailureSpec -> [Trace] -> Formula
ldfi fs = fixpoint . (failureSpecConstraint fs :&&) . Neg . lineage
