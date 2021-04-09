{-# language OverloadedStrings #-}
module Ltl where

import Data.Maybe (fromMaybe)
import qualified Data.Aeson as Aeson
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NE
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Foldable (toList)
import Data.Text (Text)
import qualified Data.HashMap.Strict as H

import Ltl.Json
import Ltl.Prop
import Ltl.Traces
import Ltl.Proof

--------------------------------------------------------------------------------


evalExpr :: CheckState -> StateBehaviour -> Expr -> Json
evalExpr _ _ (Constant js) = js
evalExpr state sb (Variable (Var t n e)) =
  let
    State s = case t of
        Before -> before sb
        After  -> after sb
  in jq e (fromMaybe Aeson.Null $ Map.lookup (evalNode (nodeEnv state) n) s)
evalExpr state _ (IntLang e) = Aeson.Number $ fromInteger $ evalInt (intEnv state) e

checkPredicate :: CheckState -> Predicate -> StateBehaviour -> Dec
checkPredicate state p sb = case p of
  Eq e e' ->
    let lhs = evalExpr state sb e
        rhs = evalExpr state sb e'
    in if lhs == rhs then Yes (PP (PEq lhs)) else No (RP (REq lhs rhs))


--------------------------------------------------------------------------------


futures :: NonEmpty a -> [NonEmpty a]
futures (t@(_ :| [])) = [t]
futures (t :| (t':ts)) = let f = futures (t' :| ts)
                             h = case f of
                               [] -> []
                               (x:xs) -> toList x
 in (t :| h) : f

data CheckState = CheckState
  { nodeEnv :: NodeEnvironment,
    intEnv :: IntEnvironment,
    nodes :: [Node]
  }

updateIenv :: IntVar -> Integer -> CheckState -> CheckState
updateIenv b i (CheckState nenv ienv ns) = CheckState nenv (Map.insert b i ienv) ns

updateNenv :: NodeVar -> Node -> CheckState -> CheckState
updateNenv b n (CheckState nenv ienv ns) = CheckState (Map.insert b n nenv) ienv ns

check' :: CheckState -> Formula -> Trace -> Dec
check' state formula ts = case formula of
  P pred -> checkPredicate state pred (NE.head ts)
  Always f ->
    allDec (check' state f) (futures ts) (RAlways . worldTime . NE.head) PAlways
  Eventually f ->
    anyDec (check' state f) (futures ts) (PEventually . worldTime . NE.head) REventually
  ForallNode b f ->
    allDec (\n -> check' (updateNenv b n state) f ts) (nodes state) RForallNode PForallNode
  ExistsNode b f ->
    anyDec (\n -> check' (updateNenv b n state) f ts) (nodes state) PExistsNode RExistsNode
  ForallInt is b f ->
    allDec (\i -> check' (updateIenv b i state) f ts) is RForallInt PForallInt
  ExistsInt is b f ->
    anyDec (\i -> check' (updateIenv b i state) f ts) is PExistsInt RExistsInt
  Imp f g -> (check' state f ts) `impDec` check' state g ts
  And f g -> check' state f ts `andDec` check' state g ts
  Or  f g -> check' state f ts `orDec` check' state g ts
  Neg f -> notDec (check' state f ts)
  TT -> Yes PTT
  FF -> No RFF

check :: Formula -> Trace -> Dec
check f t = check' state (concreteNodes (nodesFromTrace t) f) t
  where
    state = CheckState Map.empty Map.empty (nodesFromTrace t)
    nodesFromTrace (StateBehaviour (State x) _ _ _ :| _) = Map.keys x
--------------------------------------------------------------------------------

exampleTrace :: Trace
exampleTrace =
  sb 0 [(a, sa1), (b, sb1)] [(a,sa2), (b,sb2)] :| [
  sb 1 [(a, sa2), (b, sb2)] [(a,sa3), (b,sb3)],
  sb 2 [(a, sa3), (b, sb3)] [(a,sa4), (b,sb4)]]
  where
    sb i x y = StateBehaviour (State (Map.fromList x)) i Event (State (Map.fromList y))
    a = "NodeA"
    b = "NodeB"
    sa1 = Aeson.object [("state", Aeson.Bool True)]
    (sa2:sa3:sa4:_) = repeat sa1
    sb1 = Aeson.object [("bstate", Aeson.Bool True)]
    (sb2:sb3:_) = repeat sb1
    sb4 = Aeson.object [("bstate", Aeson.Bool False)]
