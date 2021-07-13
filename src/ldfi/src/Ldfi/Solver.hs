module Ldfi.Solver where

import Data.Map (Map)
import Ldfi.Prop

------------------------------------------------------------------------

-- * Solver

data Solution key = NoSolution | Solution (Map key Bool)
  deriving (Show)

data Solver key m = Solver
  {solve :: FormulaF key -> m (Solution key)}
