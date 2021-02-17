{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Ldfi where

import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import qualified GHC.Natural as Nat
import Text.Read (readMaybe)

import Ldfi.FailureSpec
import Ldfi.Prop
import Ldfi.Solver
import Ldfi.Storage
import Ldfi.Traces
import Ldfi.Util

------------------------------------------------------------------------
-- * Lineage-driven fault injection

data LDFIVar
  = EventVar Event
  | FaultVar Fault
  deriving (Eq, Ord, Read, Show)

type LDFIFormula = FormulaF LDFIVar

lineage :: [Trace] -> LDFIFormula
lineage ts =
  Or [ makeVars t | t <- map (foldMap $ Set.singleton . EventVar) ts ]

affects :: Fault -> Event -> Bool
affects (Omission (s, r) ra) (Event s' _ r' ra') = s == s' && r == r' && ra == ra'
affects (Crash n a) (Event s sa r ra) =
    (n == s && a <= sa) || (n == r && a <= ra)
   -- we should be able to be smarter if we knew when something was sent, then
   -- we could also count for the sender..

-- failureSemantic computes a formula s.t for each event either (xor) the event
-- is true or one of the failures that affect it is true
failureSemantic :: Set Event -> Set Fault -> LDFIFormula
failureSemantic events failures = And
  [ Var (EventVar event) :+ Or [ Var (FaultVar failure)
                               | failure <- Set.toList failures
                               , failure `affects` event]
  | event <- Set.toList events
  ]

-- failureSpecConstraint is the formula that makes sure we are following the
-- Failure Specification. Although we will never generate any faults that occur
-- later than eff, so we don't have to check that here
failureSpecConstraint :: FailureSpec -> Set Event -> Set Fault -> LDFIFormula
failureSpecConstraint fs events faults
  | null crashes = TT
  | otherwise    = And (AtMost crashVars (Nat.naturalToInt $ maxCrashes fs) : crashConditions)
  where
    crashes = [ c | c@(Crash _ _) <- Set.toList faults ]
    crashVars = map FaultVar crashes
    crashConditions = [ Var (FaultVar c) :-> Or [ Var (EventVar e)
                                          | e@(Event s sa _ _) <- Set.toList events
                                          , s == n && sa < t]
                      | c@(Crash n t) <- crashes]

-- enumerateAllFaults will generate the interesting faults that could affect the
-- set of events. But since it is pointless to generate a fault that is later
-- than eff (since it would never be picked anyway) we don't generate those. In
-- general it is enough to have a possible fault of either an omission or a
-- crash before the event happened.
--
-- Though for crashes they could be generated before the eff, but have no event
-- for that node in eff, so we make a special case to add a crash in eff. We
-- could have done this by always adding crash at eff for each node, but instead
-- only do this if there are events to the node after eff.
enumerateAllFaults :: Set Event -> FailureSpec -> Set Fault
enumerateAllFaults events fs = Set.unions (Set.map possibleFailure events)
  where
    eff :: Time
    eff = endOfFiniteFailures fs

    possibleFailure :: Event -> Set Fault
    possibleFailure (Event s sa r ra)
      | eff < ra  = Set.fromList [Crash s eff , Crash r eff]
      | otherwise = Set.fromList [Omission (s, r) ra, Crash s sa, Crash r ra]

-- ldfi will produce a formula that if solved will give you:
-- * Which faults to introduce
-- * Which events will break (though we are probably not interested in those)
-- such that
-- * If we introduce faults the corresponding events are false (`failureSemantic`)
-- * We don't intruduce faults that violates the failure spec (`failureSpecConstaint`)
-- * The lineage graph from traces are not satisfied (`Neg lineage`)
ldfi :: FailureSpec -> [Trace] -> FormulaF String
ldfi fs ts = fmap show $ simplify $ And
  [ failureSemantic allEvents allFaults
  , failureSpecConstraint fs allEvents allFaults
  , Neg (lineage ts)
  ]
  where
    allEvents = Set.unions (map Set.fromList ts)
    allFaults = enumerateAllFaults allEvents fs

------------------------------------------------------------------------
-- * Main

data Fault = Crash Node Time | Omission Edge Time
  deriving (Eq, Ord, Read, Show)

makeFaults :: Solution -> [Fault]
makeFaults NoSolution        = []
makeFaults (Solution assign) =
  [ f
  | (key, True) <- Map.toAscList assign
  , Just (FaultVar f) <- pure $ readMaybe key
  ]

marshalFaults :: [Fault] -> Text
marshalFaults  fs = "{\"faults\":" <> marshalList (map marshalFault fs) <> "}"
  where
    marshalList :: [Text] -> Text
    marshalList ts = "[" <> T.intercalate "," ts <> "]"

    marshalFault :: Fault -> Text
    marshalFault (Crash from at) =
      "{\"kind\":\"crash\",\"from\":" <> T.pack (show from) <> ",\"at\":" <> T.pack (show at) <> "}"
    marshalFault (Omission (from, to) at) =
      "{\"kind\":\"omission\",\"from\":" <> T.pack (show from) <> ",\"to\":" <> T.pack (show to) <>
      ",\"at\":" <> T.pack (show at) <> "}"

run :: Monad m => Storage m -> Solver m -> TestId -> FailureSpec -> m [Fault]
run Storage{load} Solver{solve} testId failureSpec = do
  traces <- load testId
  sol <- solve (ldfi failureSpec traces)
  return (makeFaults sol)

run' :: Monad m => Storage m -> Solver m -> TestId -> FailureSpec -> m Text
run' = fmap marshalFaults .:: run
