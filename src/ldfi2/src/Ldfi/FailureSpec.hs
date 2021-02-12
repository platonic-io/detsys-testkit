module Ldfi.FailureSpec where

import Numeric.Natural

import Ldfi.Traces (Time)

------------------------------------------------------------------------
-- * Failure specification

data FailureSpec = FailureSpec
  { endOfFiniteFailures :: Time    -- ^ When finite failures, i.e. omissions,
                                   -- stop (a.k.a. EOF).
  , maxCrashes          :: Natural -- ^ The maximum amount of crashes allowed.
  , endOfTime           :: Time    -- ^ When the test stops (a.k.a. EOT).
  }
