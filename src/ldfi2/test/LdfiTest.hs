module LdfiTest where

import Test.HUnit
import qualified Test.QuickCheck as QC

import Ldfi
import Ldfi.FailureSpec
import Ldfi.Prop
import Ldfi.Sat
import Ldfi.Traces
import Ldfi.Solver

------------------------------------------------------------------------

emptyFailureSpec :: FailureSpec
emptyFailureSpec = FailureSpec
  { endOfFiniteFailures = 0
  , maxCrashes = 0
  , endOfTime = 0
  }

data WasSame = Same | NotSame (Maybe String)
  deriving Show

z3_same :: Formula -> Formula -> IO WasSame
z3_same l r = do
  sol <- z3Solve (Neg (l :<-> r))
  pure $ case sol of
    Solution assignment -> NotSame (Just (show assignment))
    NoSolution          -> Same

shouldBe :: Formula -> Formula -> Assertion
shouldBe actual expected = do
  result <- z3_same actual expected
  case result of
    Same -> pure ()
    NotSame modString -> assertFailure msg
      where msg = "expected: " ++ show expected ++ "\n but got: " ++ show actual ++
              "\n model: " ++ show modString

------------------------------------------------------------------------
-- Sanity checks for z3_same

unit_z3_same_eq :: Assertion
unit_z3_same_eq = do
  r <- z3_same (Var "A") (Var "A")
  case r of
    Same -> pure ()
    _ -> assertFailure msg
      where msg = "A was not equal to it self:\n" ++ show r

unit_z3_same_neq :: Assertion
unit_z3_same_neq = do
  r <- z3_same (Var "A") (Var "B")
  case r of
    NotSame _ -> pure ()
    _ -> assertFailure msg
      where msg = "A was equal to B:\n" ++ show r

------------------------------------------------------------------------

-- Peter Alvaro's cache example: "a frontend A depends on a service B
-- and a cache C. A typical cache hit might reveal edges like (A, B),
-- (A,C). but in the event of a cache miss, the trace will reveal the
-- computation that rehydrates the cache; eg (A, B), (A, R), (R, S1),
-- (R,S2). now the system has learned of a disjunction; for success, it
-- appears that we require A, B, and (C or (R,S1,S2)). after just a few
-- executions, LDFI is leveraging redundancy and not just injecting
-- faults arbitrarily. does that make sense? LDFI will never, e.g.,
-- suggest failing S1 or S2 or both without also suggesting failing C."
cacheTraces :: [Trace]
cacheTraces =
  [ [Event "A" "B" 0, Event "A" "C" 1]
  , [Event "A" "B" 0, Event "A" "R" 1, Event "R" "S1" 2, Event "R" "S2" 3]
  ]

unit_cache :: Assertion
unit_cache =
    (ldfi emptyFailureSpec cacheTraces) `shouldBe`
    (Neg (And [Var "A", Var "B"] :&& (Var "C" :|| And [Var "R", Var "S1", Var "S2"])))

------------------------------------------------------------------------

-- Node A broadcasts to node B and C without retry. Node B getting the
-- message constitutes a successful outcome.
broadcast1Traces :: [Trace]
broadcast1Traces = [ [Event "A" "B" 1, Event "A" "C" 1]
                   , [Event "A" "B" 1] -- Omission between A C or Crash C.
                   ]

unit_broadcast1 :: Assertion
unit_broadcast1 =
  (ldfi emptyFailureSpec broadcast1Traces) `shouldBe`
  (Neg (And [Var "A", Var "B"]))
  -- ^ XXX: If the SAT solver keeps finding crashing C as the solution
  -- then we are stuck in a loop?

------------------------------------------------------------------------

-- Node A broadcasts to node B and C with retry. Node B getting the
-- message constitutes a successful outcome.
broadcast2Traces :: [Trace]
broadcast2Traces = [ [Event "A" "B" 1, Event "A" "C" 1]
                   , [Event "A" "B" 1] -- Omission between A C or Crash C.
                   ]


------------------------------------------------------------------------
-- QuickCheck property

-- simplify gives a logical equivalent formula
prop_simplify_eq :: Formula -> QC.Property
prop_simplify_eq f =
  let simp_f = simplify1 f
  in QC.ioProperty $ do
    res <- z3_same f simp_f
    return $ case res of
      Same -> True
      _ -> False
