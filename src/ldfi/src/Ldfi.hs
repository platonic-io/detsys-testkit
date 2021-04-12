{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Ldfi where

import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as BSL
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text.Encoding as TE
import qualified GHC.Natural as Nat
import Ldfi.FailureSpec
import qualified Ldfi.Marshal.Faults as MF
import Ldfi.Prop
import Ldfi.Solver
import Ldfi.Storage
import Ldfi.Traces
import Text.Read (readMaybe)

------------------------------------------------------------------------

-- * Lineage-driven fault injection

data LDFIVar
  = EventVar Event
  | FaultVar Fault
  deriving (Eq, Ord, Read, Show)

type LDFIFormula = FormulaF LDFIVar

lineage :: [Trace] -> LDFIFormula
lineage ts =
  Or [makeVars t | t <- map (foldMap $ Set.singleton . EventVar) ts]

affects :: Fault -> Event -> Bool
affects (Omission (s, r) ra) (Event s' _ r' ra') = s == s' && r == r' && ra == ra'
affects (Crash n a) (Event s sa r ra) =
  (n == s && a <= sa) || (n == r && a <= ra)

-- failureSemantic computes a formula s.t for each event either (xor) the event
-- is true or one of the failures that affect it is true
failureSemantic :: Set Event -> Set Fault -> LDFIFormula
failureSemantic events faults =
  And
    [ Var (EventVar event)
        :+ Or
          [ Var (FaultVar failure)
            | failure <- Set.toList faults,
              failure `affects` event
          ]
      | event <- Set.toList events
    ]

-- failureSpecConstraint is the formula that makes sure we are following the
-- Failure Specification. Although we will never generate any faults that occur
-- later than eff, so we don't have to check that here
failureSpecConstraint :: FailureSpec -> Set Event -> Set Fault -> LDFIFormula
failureSpecConstraint fs events faults
  | null crashes = TT
  | otherwise = And (AtMost crashVars (Nat.naturalToInt $ maxCrashes fs) : crashConditions)
  where
    crashes = [c | c@(Crash _ _) <- Set.toList faults]
    crashVars = map FaultVar crashes
    crashConditions =
      [ Var (FaultVar c)
          :-> Or
            [ Var (EventVar e)
              | e@(Event s sa _ _) <- Set.toList events,
                s == n && sa < t
            ]
        | c@(Crash n t) <- crashes
      ]

-- `enumerateAllFaults` will generate the interesting faults that could affect
-- the set of events. But since it is pointless to generate a fault that is
-- later than eff (since it would never be picked anyway) we don't generate
-- those. In general it is enough to have a possible fault of either an omission
-- or a crash before the event happened.
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
      | eff < ra = Set.fromList [Crash s eff, Crash r eff]
      | otherwise = Set.fromList [Omission (s, r) ra, Crash s sa, Crash r ra]

-- | `ldfi` will produce a formula that if solved will give you:
--     * Which faults to introduce
--     * Which events will break (though we are probably not interested in those)
-- such that
--     * If we introduce faults the corresponding events are false (`failureSemantic`)
--     * We don't intruduce faults that violates the failure spec (`failureSpecConstaint`)
--     * The lineage graph from traces are not satisfied (`Neg lineage`)
ldfi :: FailureSpec -> Map RunId Trace -> Failures -> FormulaF String
ldfi failureSpec tracesByRuns failures =
  fmap show $
    simplify $
      And $
        addLimit
          [ failureSemantic allEvents allFaults,
            failureSpecConstraint failureSpec allEvents allFaults,
            Neg (lineage traces),
            And
              [ Neg $ And $ map (Var . FaultVar) faults
                | faults <- fFaultsFromFailedRuns failures
              ]
          ]
  where
    addLimit xs = case numberOfFaultLimit failureSpec of
      Nothing -> xs
      Just l -> AtMost [FaultVar $ f | f <- Set.toList allFaults] l : xs
    traces = Map.elems tracesByRuns
    allEvents = Set.unions (map Set.fromList traces)
    allFaults = enumerateAllFaults allEvents failureSpec

------------------------------------------------------------------------

-- * Main

makeFaults :: Solution -> [Fault]
makeFaults NoSolution = []
makeFaults (Solution assign) =
  [ f
    | (key, True) <- Map.toAscList assign,
      Just (FaultVar f) <- pure $ readMaybe key
  ]

marshalFaults :: [Fault] -> Text
marshalFaults = TE.decodeUtf8 . BSL.toStrict . Aeson.encode . MF.Faults . map convert
  where
    convert :: Fault -> MF.Fault
    convert (Crash f a) = MF.Crash f a
    convert (Omission (f, t) a) = MF.Omission f t a

run :: Monad m => Storage m -> Solver m -> TestInformation -> FailureSpec -> m [Fault]
run Storage {load, loadFailures} Solver {solve} testInformation failureSpec = do
  traces <- load testInformation
  failures <- loadFailures testInformation
  sol <- solve (ldfi failureSpec traces failures)
  return (makeFaults sol)

run' :: Monad m => Storage m -> Solver m -> TestInformation -> FailureSpec -> m Text
run' storage solver testInformation failureSpec = fmap marshalFaults (run storage solver testInformation failureSpec)
