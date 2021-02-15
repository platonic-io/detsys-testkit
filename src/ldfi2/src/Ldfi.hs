{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE NamedFieldPuns #-}

module Ldfi where

import qualified Data.Set as Set

import Ldfi.Storage
import Ldfi.FailureSpec
import Ldfi.Prop
import Ldfi.Traces
import Ldfi.Solver

------------------------------------------------------------------------
-- * Lineage-driven fault injection

lineage :: [Trace] -> Formula
lineage ts =
  -- Or [ makeVars t | t <- map nodes ts]
  let
    ns  = map nodes ts
    is  = foldl1 Set.intersection ns
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

------------------------------------------------------------------------
-- * Main

run :: Monad m => Storage m -> Solver m -> TestId -> FailureSpec -> m Solution
run Storage{load} Solver{solve} testId failureSpec = do
  traces <- load testId
  solve (ldfi failureSpec traces)
