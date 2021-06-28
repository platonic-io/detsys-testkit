module StuntDouble.Metrics where

import GHC.Float
import GHC.Natural

import StuntDouble.Histogram

------------------------------------------------------------------------

data Metrics = Metrics
  { mEventLoopSat :: Histogram }

newMetrics :: IO Metrics
newMetrics = Metrics
  <$> newHistogram

reportEventLoopDepth :: Natural -> Metrics -> IO Int
reportEventLoopDepth depth m =
  measure (int2Double (naturalToInt depth)) (mEventLoopSat m)
