{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Scheduler.Fault where

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Maybe (fromMaybe)

import Scheduler.Event
import qualified Scheduler.Faults as Faults
import StuntDouble.LogicalTime
import StuntDouble.Time

-- XXX unclear if we can use LogicalTime in the same sense we do for ldfi?

------------------------------------------------------------------------

type ActorName = String

data FaultStateForActor = FaultStateForActor
  { fsOmissions :: Set Int -- should be LogicalTime
  , fsPermanentCrash :: Maybe LogicalTime
  , fsPause :: TimeIntervals
  -- , fsPartition
  }
  deriving stock Show

instance Semigroup FaultStateForActor where
  FaultStateForActor o c p <> FaultStateForActor o' c' p' = FaultStateForActor (o <> o') (c `plusL` c') (p <> p')
    where
      -- this is almost (<>) for Maybe, except LogicalTime doesn't have Semigroup (we want it to be Min Int)
      plusL Nothing x = x
      plusL x Nothing = x
      plusL (Just a) (Just b) = Just $ if a `afterLogicalTime` b then b else a

instance Monoid FaultStateForActor where
  -- once again no Semigroup for LogicalTime so need to use Nothing here
  mempty = FaultStateForActor mempty Nothing mempty

newtype FaultState = FaultState (Map ActorName FaultStateForActor)
  deriving newtype Show

instance Semigroup FaultState where
  FaultState m <> FaultState m' = FaultState $ Map.unionWith (<>) m m'

instance Monoid FaultState where
  mempty = FaultState mempty

newFaultState :: Faults.Faults -> FaultState
newFaultState = foldMap mkFaultState . Faults.faults
  where
    mkFaultState :: Faults.Fault -> FaultState
    mkFaultState f = FaultState $ uncurry Map.singleton (translate f)

    nodeName = NodeName "scheduler"
    (!->) = (,)
    translate :: Faults.Fault -> (ActorName, FaultStateForActor)
    translate (Faults.Omission _f t a) = t !-> mempty { fsOmissions = Set.singleton a}
    translate (Faults.Crash f a) = f !-> mempty { fsPermanentCrash = Just $ LogicalTime nodeName{-?-} a}
    translate (Faults.Pause n f t) = n !-> mempty { fsPause = singleton (TimeInterval f t)}

------------------------------------------------------------------------
afterLogicalTime :: LogicalTime -> LogicalTime -> Bool
afterLogicalTime after before = case relation after before of
  HappenedAfter -> True
  _ -> False -- ??

shouldDrop
  :: (Time, SchedulerEvent)
  -> LogicalTime
  -> FaultState
  -> Bool {- Dropped -}
shouldDrop (t,  e) lt (FaultState fsForAll) = case Map.lookup (to e) fsForAll of
  Nothing -> False
  Just fs -> isOmitted || isCrashed || isPaused
    -- maybe we should keep the reason why it is dropped for tracing?
    -- maybe use https://hackage.haskell.org/package/explainable-predicates ?
    where
      -- t == at e ??
      isTick = kind e == "timer" -- yikes stringly typed..
      ltInt = let LogicalTime _ i = lt in i
      isOmitted = ltInt `Set.member` fsOmissions fs
        && not isTick
      isCrashed = maybe False (lt `afterLogicalTime`) (fsPermanentCrash fs)
      isPaused  = contains t $ fsPause fs

------------------------------------------------------------------------
-- does Time support inf?
data TimeInterval = TimeInterval {tiFrom :: Time, tiTo :: Time}
  deriving stock (Eq, Ord, Show)

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
  . Set.filter (\(TimeInterval a b) -> afterTime t a && afterTime b t)
  $ s
