module LdfiTest where

import Test.HUnit

import Ldfi

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
  assertEqual ""
    (ldfi cacheTraces)
    (And [Var "A", Var "B"] :&& (Var "C" :|| And [Var "R", Var "S1", Var "S2"]))

------------------------------------------------------------------------

-- Node A broadcasts to node B and C without retry. Node B getting the
-- message constitutes a successful outcome.
broadcast1Traces :: [Trace]
broadcast1Traces = [ [Event "A" "B" 1, Event "A" "C" 1]
                   , [Event "A" "B" 1] -- Omission between A C or Crash C.
                   ]

unit_broadcast1 :: Assertion
unit_broadcast1 = assertEqual ""
  (ldfi broadcast1Traces)
  (And [Var "A", Var "B"] :&& Var "C")
  -- ^ XXX: If the SAT solver keeps finding crashing C as the solution
  -- then we are stuck in a loop?

------------------------------------------------------------------------

-- Node A broadcasts to node B and C with retry. Node B getting the
-- message constitutes a successful outcome.
broadcast2Traces :: [Trace]
broadcast2Traces = [ [Event "A" "B" 1, Event "A" "C" 1]
                   , [Event "A" "B" 1] -- Omission between A C or Crash C.
                   ]
