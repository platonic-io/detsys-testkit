{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Scheduler.TimeInterval where

import Data.Set (Set)
import qualified Data.Set as Set

import StuntDouble.Time
------------------------------------------------------------------------
-- does Time support inf?
data TimeInterval = TimeInterval {tiFrom :: Time, tiTo :: Time}
  deriving stock (Eq, Ord, Show)

contain :: Time -> TimeInterval -> Bool
contain t (TimeInterval a b) = afterTime t a && afterTime b t

-- I'm sure there is some clever data structure for this
newtype TimeIntervals = TimeIntervals (Set TimeInterval)
  deriving newtype (Semigroup, Monoid, Show)

emptyIntervals :: TimeIntervals
emptyIntervals = TimeIntervals Set.empty

singleton :: TimeInterval -> TimeIntervals
singleton = TimeIntervals . Set.singleton

contains :: Time -> TimeIntervals -> Bool
contains t (TimeIntervals s) =
    not
  . Set.null
  . Set.filter (contain t)
  $ s
