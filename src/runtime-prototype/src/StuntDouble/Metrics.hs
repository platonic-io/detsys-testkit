module StuntDouble.Metrics where

import Data.Atomics.Counter
import GHC.Float
import GHC.Natural

import StuntDouble.Histogram

------------------------------------------------------------------------

data Metrics = Metrics
  { mEventLoopSat :: Histogram
  , mErrorCounter :: AtomicCounter
  }

newMetrics :: IO Metrics
newMetrics = Metrics
  <$> newHistogram
  <*> newCounter 0

reportEventLoopDepth :: Natural -> Metrics -> IO Int
reportEventLoopDepth depth m =
  measure (int2Double (naturalToInt depth)) (mEventLoopSat m)

reportError :: Metrics -> IO ()
reportError = incrCounter_ 1 . mErrorCounter
