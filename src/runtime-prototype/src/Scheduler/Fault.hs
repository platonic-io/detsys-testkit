{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Scheduler.Fault where

import Data.List (foldl')
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Maybe (fromMaybe)
import qualified Data.Time as Time

import Scheduler.Event
import qualified Scheduler.Faults as Faults
import StuntDouble.LogicalTime
import StuntDouble.Time

-- XXX unclear if we can use LogicalTime in the same sense we do for ldfi?

------------------------------------------------------------------------

type ActorName = String

{-
  In Jepsen they are generated:
  CSABump:
    oneOf(-1,1)*2^(2+rand(16)) seconds
  CSAStrobe
   delta: 2^(2+rand(16)) milliseconds
   period: 2^rand(10) milliseconds
-}
data ClockSkewAction
  = CSABump Time.NominalDiffTime
  | CSAStrobe
    { csaDelta :: Time.NominalDiffTime
    , csaPeriod :: Time.NominalDiffTime
    }
  deriving (Eq, Show)

newtype ClockSkew = ClockSkew
  { csSkews :: [(TimeInterval, ClockSkewAction)]
  } deriving newtype (Semigroup, Monoid, Show)

data FaultStateForActor = FaultStateForActor
  { fsOmissions :: Set Int -- should be LogicalTime
  , fsPermanentCrash :: Maybe LogicalTime
  , fsPause :: TimeIntervals
  , fsPartition :: Map ActorName TimeIntervals
  , fsClockSkew :: ClockSkew
  }
  deriving stock Show

instance Semigroup FaultStateForActor where
  FaultStateForActor o c p pa cs <> FaultStateForActor o' c' p' pa' cs'
    = FaultStateForActor (o <> o') (c `plusL` c') (p <> p') (Map.unionWith (<>) pa pa') (cs <> cs')
    where
      -- this is almost (<>) for Maybe, except LogicalTime doesn't have Semigroup (we want it to be Min Int)
      plusL Nothing x = x
      plusL x Nothing = x
      plusL (Just a) (Just b) = Just $ if a `afterLogicalTime` b then b else a

instance Monoid FaultStateForActor where
  -- once again no Semigroup for LogicalTime so need to use Nothing here
  mempty = FaultStateForActor mempty Nothing mempty mempty mempty

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
    translate (Faults.Partition n c f t) = n !-> mempty { fsPartition = Map.fromList $ zip c (repeat ti) }
     where ti = singleton $ TimeInterval f t
    translate (Faults.ClockSkewBump n d f t) = n !-> mempty { fsClockSkew = ClockSkew [(TimeInterval f t, CSABump d)]}
    translate (Faults.ClockSkewStrobe n d p f t) = n !-> mempty { fsClockSkew = ClockSkew [(TimeInterval f t, CSAStrobe d p)]}

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
  Just fs -> isOmitted || isCrashed || isPaused || isPartition
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
      isPartition = maybe False (contains t) $ Map.lookup (from e) (fsPartition fs)

applySkew :: Time -> TimeInterval -> ClockSkewAction -> Time
applySkew (Time t) (TimeInterval {tiFrom = Time tf}) (CSAStrobe delta period) = addTime (Time t) d'
  where
    t' = Time.diffUTCTime t tf
    b = floor (Time.nominalDiffTimeToSeconds (t' / period)) `mod` 2
    d' = if b == 0 then delta else -delta


manipulateEvent
  :: SchedulerEvent
  -> FaultState
  -> SchedulerEvent
manipulateEvent e (FaultState fsAll) = case Map.lookup (to e) fsAll of
  Nothing -> e
  Just fs -> e { at = foldl' update (at e) (csSkews (fsClockSkew fs))}
    where
      update :: Time -> (TimeInterval, ClockSkewAction) -> Time
      update t (ti, action)
        | contain t ti = applySkew t ti action
        | otherwise = t

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
