{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Scheduler.Fault where

import qualified Data.Aeson as Aeson
import Data.List (foldl')
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Maybe (fromMaybe)
import qualified Data.Time as Time

import qualified Scheduler.Agenda as Agenda
import Scheduler.Agenda (Agenda)
import Scheduler.Event
import qualified Scheduler.Faults as Faults
import Scheduler.Executor (UnscheduledEvent(..), fromUE)
import Scheduler.TimeInterval
import StuntDouble.LogicalTime
import StuntDouble.Random
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

-- RandomVariable for a boolean in the [0,1] outcomespace
-- both `rvPoint` and `rvRange` are in [0,1]
-- `rvRange` is how often it should be true, and `rvPoint` is some deteriminstic randomness
data RandomVariable
  = RandomVariable
    { rvPoint :: RandomInterval
    , rvRange :: Double
    }
  deriving (Show)

-- The semantic function of `RandomVariable`
randomVariable :: RandomVariable -> RandomInterval -> Bool
randomVariable rv d = (d `add` rvPoint rv) `isLessThan` rvRange rv

data DuplicationInfo
  = DuplicationInfo
    { diRandomVariable :: RandomVariable -- [0,1]
    , diDeltaNs :: Time.NominalDiffTime
    }
  deriving (Show)

newtype DuplicationPotential = DuplicationPotential
  {dpZones :: [(TimeInterval, DuplicationInfo)]
  } deriving newtype (Semigroup, Monoid, Show)

data FaultStateForActor = FaultStateForActor
  { fsOmissions :: Set LogicalTimeInt
  , fsPermanentCrash :: Maybe LogicalTime
  , fsPause :: TimeIntervals
  , fsPartition :: Map ActorName TimeIntervals
  , fsClockSkew :: ClockSkew
  , fsDuplicationPotential :: DuplicationPotential
  }
  deriving stock Show

instance Semigroup FaultStateForActor where
  FaultStateForActor o c p pa cs dp <> FaultStateForActor o' c' p' pa' cs' dp'
    = FaultStateForActor (o <> o') (c `plusL` c') (p <> p') (Map.unionWith (<>) pa pa') (cs <> cs') (dp <> dp')
    where
      -- this is almost (<>) for Maybe, except LogicalTime doesn't have Semigroup (we want it to be Min Int)
      plusL Nothing x = x
      plusL x Nothing = x
      plusL (Just a) (Just b) = Just $ if a `afterLogicalTime` b then b else a

instance Monoid FaultStateForActor where
  -- once again no Semigroup for LogicalTime so need to use Nothing here
  mempty = FaultStateForActor mempty Nothing mempty mempty mempty mempty

newtype FaultState = FaultState (Map ActorName FaultStateForActor)
  deriving newtype Show

instance Semigroup FaultState where
  FaultState m <> FaultState m' = FaultState $ Map.unionWith (<>) m m'

instance Monoid FaultState where
  mempty = FaultState mempty

newFaultState :: Faults.Faults -> (FaultState, Agenda)
newFaultState = foldMap mkFaultState . Faults.faults
  where
    mkFaultState :: Faults.Fault -> (FaultState, Agenda)
    mkFaultState f = (FaultState . fromMaybe mempty $ uncurry Map.singleton <$> (translate f)
                     , agendaItems f)

    nodeName = NodeName "scheduler"
    k !-> v = Just (k,v)

    translate :: Faults.Fault -> Maybe (ActorName, FaultStateForActor)
    translate (Faults.Omission _f t a) = t !-> mempty { fsOmissions = Set.singleton a}
    translate (Faults.Crash f a) = f !-> mempty { fsPermanentCrash = Just $ LogicalTime nodeName{-?-} a}
    translate (Faults.Pause n f t) = n !-> mempty { fsPause = singleton (TimeInterval f t)}
    translate (Faults.Partition n c f t) = n !-> mempty { fsPartition = Map.fromList $ zip c (repeat ti) }
     where ti = singleton $ TimeInterval f t
    translate (Faults.ClockSkewBump n d f t) = n !-> mempty { fsClockSkew = ClockSkew [(TimeInterval f t, CSABump d)]}
    translate (Faults.ClockSkewStrobe n d p f t) = n !-> mempty { fsClockSkew = ClockSkew [(TimeInterval f t, CSAStrobe d p)]}
    translate Faults.RestartReactor{} = Nothing
    translate (Faults.DuplicateMessage n f t d p r) =  n !-> mempty { fsDuplicationPotential = DuplicationPotential [(TimeInterval f t, DuplicationInfo (RandomVariable (detRandomInterval p) r) d)]}

    agendaItems :: Faults.Fault -> Agenda
    agendaItems Faults.Omission{} = mempty
    agendaItems Faults.Crash{} = mempty
    agendaItems Faults.Pause{} = mempty
    agendaItems Faults.Partition{} = mempty
    agendaItems Faults.ClockSkewBump{} = mempty
    agendaItems Faults.ClockSkewStrobe{} = mempty
    agendaItems (Faults.RestartReactor n t) = Agenda.push (t, ev) mempty
      where
        ev = SchedulerEvent {kind = "fault", event = "restart", args = Aeson.object [], from = "god", to = n, at = t, meta = Nothing}
    agendaItems Faults.DuplicateMessage{} = mempty

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

manipulateOutGoingEvent
  :: UnscheduledEvent
  -> FaultState
  -> Time
  -> RandomInterval
  -> [UnscheduledEvent]
manipulateOutGoingEvent ue (FaultState fsAll) at d = ue : dups (Map.lookup (fromUE ue) fsAll)
  where
    dups Nothing = []
    dups (Just fs)
      = [ UEDelayed ue (diDeltaNs di)
        | (ti, di) <- dpZones (fsDuplicationPotential fs)
        , contain at ti
        , randomVariable (diRandomVariable di) d
        ]
