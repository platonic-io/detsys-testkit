{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Scheduler.Fault where

import Data.Heap (Entry(Entry))
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


data FaultState = FaultState
  { fsOmissions :: Map ActorName (Set Int) -- should be LogicalTime
  , fsPermanentCrash :: Map ActorName LogicalTime
  , fsPause :: Map ActorName TimeIntervals
  -- , fsPartition
  }
  deriving stock Show

instance Semigroup FaultState where
  FaultState o c p <> FaultState o' c' p' = FaultState (o `plus` o') (c `plusL` c') (p `plus` p')
    where
      plus a b = Map.unionWith (<>) a b -- eta-expandend to defeat the monomorphism-restriction
      plusL = Map.unionWith $ \ a b -> if a `afterLogicalTime` b then b else a

instance Monoid FaultState where
  mempty = FaultState mempty mempty mempty

newFaultState :: Faults.Faults -> FaultState
newFaultState = foldMap translate . Faults.faults
  where
    nodeName = NodeName "scheduler"
    translate :: Faults.Fault -> FaultState
    translate (Faults.Omission _f t a) = mempty { fsOmissions = Map.singleton t $ Set.singleton a}
    translate (Faults.Crash f a) = mempty { fsPermanentCrash = Map.singleton f $ LogicalTime nodeName a} -- ?


------------------------------------------------------------------------
afterLogicalTime :: LogicalTime -> LogicalTime -> Bool
afterLogicalTime after before = case relation after before of
  HappenedAfter -> True
  _ -> False -- ??

shouldDrop
  :: Entry Time SchedulerEvent
  -> LogicalTime
  -> FaultState
  -> Bool {- Dropped -}
shouldDrop (Entry t e) lt fs = isOmitted || isCrashed || isPaused
  -- maybe we should keep the reason why it is dropped for tracing?
  -- maybe use https://hackage.haskell.org/package/explainable-predicates ?
  where
    -- t == at e ??
    actor = to e
    isTick = kind e == "timer" -- yikes stringly typed..
    ltInt = let LogicalTime _ i = lt in i
    isOmitted = ltInt `Set.member` fromMaybe Set.empty (Map.lookup actor (fsOmissions fs))
      && not isTick
    isCrashed = maybe False (lt `afterLogicalTime`) (Map.lookup actor $ fsPermanentCrash fs)
    isPaused  = contains t $ fromMaybe emptyIntervals (Map.lookup actor $ fsPause fs)

------------------------------------------------------------------------
-- does Time support inf?
data TimeInterval = TimeInterval {tiFrom :: Time, tiTo :: Time}
  deriving stock (Eq, Ord, Show)

-- I'm sure there is some clever data structure for this
newtype TimeIntervals = TimeIntervals (Set TimeInterval)
  deriving newtype (Semigroup, Monoid, Show)


emptyIntervals :: TimeIntervals
emptyIntervals = TimeIntervals Set.empty

contains :: Time -> TimeIntervals -> Bool
contains t (TimeIntervals s) =
    not
  . Set.null
  . Set.filter (\(TimeInterval a b) -> afterTime t a && afterTime b t)
  $ s
