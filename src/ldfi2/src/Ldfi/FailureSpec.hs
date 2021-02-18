module Ldfi.FailureSpec where

import Ldfi.Traces (Time)
import Numeric.Natural

------------------------------------------------------------------------

-- * Failure specification

data FailureSpec = FailureSpec
  { -- | When finite failures, i.e. omissions, stop (a.k.a. EOF).
    endOfFiniteFailures :: Time,
    -- | The maximum amount of crashes allowed.
    maxCrashes :: Natural,
    -- | When the test stops (a.k.a. EOT).
    endOfTime :: Time
  }
