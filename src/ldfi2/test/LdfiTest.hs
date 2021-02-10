module LdfiTest where

import Test.HUnit

import Ldfi

------------------------------------------------------------------------

unit_cache :: Assertion
unit_cache =
  assertEqual ""
    (ldfi exTraces)
    (And [Var "A",Var "B"] :&& (Var "C" :|| And [Var "R",Var "S1",Var "S2"]))
