module Ltl.Proof where

import Ltl.Json
import Ltl.Traces

{-
We might want to store more information in the predicates but this is a start
-}
data PredicateProof = PEq Json -- the value both were equal to
  deriving (Eq, Show)

data PredicateRefutation = REq Json Json -- the two different values
  deriving (Eq, Show)

data Proof
  = PP PredicateProof
  | PAlways -- we intentionally don't have proof for all the subterms
  | PEventually Int Proof -- the world it was true in, and proof it was true
  | PForallNode -- intentionally empty
  | PExistsNode Node Proof -- node, and proof this is true for this node
  | PForallInt -- intentionally empty
  | PExistsInt Integer Proof -- Int and proof that the int satisfy the formula
  | PImp (Either Refutation Proof) -- Proof (p -> q) = Refutation p + Proof q
  | PAnd Proof Proof -- Proof (p /\ q) = Proof p * Proof q
  | POr (Either Proof Proof) -- Proof (p \/ q) = Proof p + Proof q
  | PNeg Refutation -- Proof (~ p) = Refutation p
  | PTT -- P tt = {}
  deriving (Eq, Show)

data Refutation
  = RP PredicateRefutation
  | RAlways Int Refutation -- R ([] p) =  w * R p @ w
  | REventually -- R (<> p) = {}
  | RForallNode Node Refutation -- R (\forall n. p) = N * R (p[n:=N])
  | RExistsNode -- R (\exists n. p) = {}
  | RForallInt Integer Refutation -- R (\forall x. p) = i * R (p [x:=i])
  | RExistsInt -- R (\exists x. p) = {}
  | RImp Proof Refutation -- R (p -> q) = P p * R q
  | RAnd (Either Refutation Refutation) -- R (p /\ q) = R p + R q
  | ROr Refutation Refutation -- R (p \/ q) = R p * R q
  | RNeg Proof -- R (~ p) = P p
  | RFF -- R ff = {}
  deriving (Eq, Show)

data Dec
  = Yes Proof
  | No Refutation
  deriving (Eq, Show)

notDec :: Dec -> Dec
notDec (Yes p) = No (RNeg p)
notDec (No r) = Yes (PNeg r)

andDec :: Dec -> Dec -> Dec
andDec (No r) _ = No (RAnd (Left r))
andDec _ (No r) = No (RAnd (Right r))
andDec (Yes p) (Yes p') = Yes (PAnd p p')

orDec :: Dec -> Dec -> Dec
orDec (Yes p) _ = Yes (POr (Left p))
orDec _ (Yes p) = Yes (POr (Right p))
orDec (No r) (No r') = No (ROr r r')

impDec :: Dec -> Dec -> Dec
impDec _ (Yes p) = Yes (PImp (Right p))
impDec (No r) _ = Yes (PImp (Left r))
impDec (Yes p) (No r) = No (RImp p r)

allDec :: (r -> Dec) -> [r] -> (r -> Refutation -> Refutation) -> Proof -> Dec
allDec pred xs ref pro = go xs
  where
    go [] = Yes pro
    go (x:xs) = case pred x of
      Yes _ -> go xs
      No r -> No $ ref x r

anyDec :: (r -> Dec) -> [r] -> (r -> Proof -> Proof) -> Refutation -> Dec
anyDec pred xs pro ref = go xs
  where
    go [] = No ref
    go (x:xs) = case pred x of
      Yes p -> Yes $ pro x p
      No _ -> go xs

reason :: Dec -> String
reason = show
