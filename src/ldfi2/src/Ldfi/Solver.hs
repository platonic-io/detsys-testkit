module Ldfi.Solver where

import Data.Map (Map)

import Ldfi.Prop

------------------------------------------------------------------------
-- * Solver

data Solution = NoSolution | Solution (Map String Bool)
  deriving Show

data Solver m = Solver
  { solve :: Formula -> m Solution }
