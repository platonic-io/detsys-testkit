-- |
-- Ported from:
-- https://github.com/palvaro/molly/blob/a3a6d7950814e1154253357a03e7d64754c464b8/src/test/scala/edu/berkeley/cs/boom/molly/FailureSpecSuite.scala
module Ldfi.EstimateTest where

import Ldfi.Estimate
import Test.HUnit

------------------------------------------------------------------------

-- * The failure free scenario

-- should only be one such scenario.
unit_estFailureFree :: Assertion
unit_estFailureFree =
  grossEstimate (Eot 3) (Eff 0) (MaxCrashes 0) nodes3 @=? 1

------------------------------------------------------------------------

-- * Fail-stop scenarios

-- should treat failures independently
unit_estTreatFailuresIndependently :: Assertion
unit_estTreatFailuresIndependently =
  grossEstimate (Eot 3) (Eff 0) (MaxCrashes 2) nodes2 @=? 16

-- should allow each node to crash once or never
unit_estCrashOnceOrNever :: Assertion
unit_estCrashOnceOrNever =
  grossEstimate (Eot 3) (Eff 0) (MaxCrashes 1) nodes1 @=? 4

unit_estCrashOnceOrNever2 :: Assertion
unit_estCrashOnceOrNever2 =
  grossEstimate (Eot 3) (Eff 0) (MaxCrashes 1) nodes2 @=? 8

unit_estCrashOnceOrNever3 :: Assertion
unit_estCrashOnceOrNever3 =
  grossEstimate (Eot 3) (Eff 0) (MaxCrashes 1) nodes3 @=? 12

-- should respect maxCrashes
unit_estRespectMaxCrashes :: Assertion
unit_estRespectMaxCrashes =
  grossEstimate (Eot 3) (Eff 0) (MaxCrashes 2) nodes4 @=? 96
-- ^ Explaination:
--
--  * There are two nodes that never crash, and there are 6 ways of picking those
--    two nodes;
--
--  * Those nodes only have one failure schedule each;
--
--  * The nodes that _are_ crash prone can crash at one of four times.
--
-- So: 6 * 1 * (4 * 4) = 96
--
-- (ways to pick which nodes don't crash) *
-- (executions of crash-free nodes) *
-- (executions of crash prone nodes)

------------------------------------------------------------------------

-- * Omission-only scenarios

-- should allow omissions until eff

unit_estOmissionsEff :: Assertion
unit_estOmissionsEff = grossEstimate (Eot 3) (Eff 2) (MaxCrashes 0) nodes2 @=? 16

unit_estOmissionsEff' :: Assertion
unit_estOmissionsEff' = grossEstimate (Eot 3) (Eff 1) (MaxCrashes 0) nodes2 @=? 4

------------------------------------------------------------------------

-- * Scenarios with both crashes and omissions

-- should prevent crashed nodes from sending messages.
unit_estCrashesPreventSending :: Assertion
unit_estCrashesPreventSending = grossEstimate (Eot 3) (Eff 2) (MaxCrashes 2) nodes2 @=? 121
-- ^ Explaination:
-- With a naive estimate that treated omissions and crashes independently, each
-- node has 4 choices of times to crash and 4 possible combinations of message
-- omissions.
--
-- There are two nodes, so we have (4 * 4) ^ 2 = 256 possible failure scenarios.
--
-- However, if we condition on when the node crashes:
--
--   * Crash @t=1  -> 1 choices of omissions
--   * crash @t=2  -> 2 choices
--   * crash @t=3  -> 4 choices
--   * Never crash -> 4 choices
--
-- So 11 ^ 2 = 121 possible failure scenarios.

unit_estNoOverflow :: Assertion
unit_estNoOverflow =
  assertBool
    "Shouldn't overflow and estimate zero scenarios"
    (grossEstimate 6 4 1 nodes5 /= 0)

------------------------------------------------------------------------

unit_estTreatFailuresIndependently' :: Assertion
unit_estTreatFailuresIndependently' =
  possibleCrashFaultsCount nodes2 (MaxCrashes 2) (NetworkTrace [1, 2, 3]) @=? 25
-- ^ NOTE: this should be 16 according to `grossEstimate`.

unit_estCrashOnceOrNever' :: Assertion
unit_estCrashOnceOrNever' =
  possibleCrashFaultsCount nodes1 (MaxCrashes 1) (NetworkTrace [1, 2, 3]) @=? 4

unit_estCrashOnceOrNever2' :: Assertion
unit_estCrashOnceOrNever2' =
  possibleCrashFaultsCount nodes2 (MaxCrashes 1) (NetworkTrace [1, 2, 3]) @=? 7

-- NOTE: should be 8.

unit_estCrashOnceOrNever3' :: Assertion
unit_estCrashOnceOrNever3' =
  possibleCrashFaultsCount nodes3 (MaxCrashes 1) (NetworkTrace [1, 2, 3]) @=? 10

-- NOTE: should be 12.
