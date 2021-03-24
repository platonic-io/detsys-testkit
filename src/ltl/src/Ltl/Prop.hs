module Ltl.Prop where

import Data.List ((\\))
import Data.Map (Map)
import qualified Data.Map as Map

import Ltl.Json (JQ, Json)
import Ltl.Traces(Node)


data VTemporal = Before | After
  deriving (Eq, Show)

type NodeVar = String -- use bound?

data NodeExpression = ConcreteNode Node | VariableNode NodeVar
  deriving (Eq, Show)

type NodeEnvironment = Map NodeVar Node

evalNode :: NodeEnvironment -> NodeExpression -> Node
evalNode _ (ConcreteNode node) = node
evalNode env (VariableNode var) = env Map.! var

bindNodeEnvironment :: NodeVar ->  Node -> NodeEnvironment -> NodeEnvironment
bindNodeEnvironment = Map.insert

data Var = Var
  { time :: VTemporal
  , node :: NodeExpression
  , expr :: JQ
  }
  deriving (Eq, Show)

type IntVar = String

data IntExpr
  = IVarAdd IntVar Integer -- you can add constant with this
  | IConst Integer
  deriving (Eq, Show)

type IntEnvironment = Map IntVar Integer

evalInt :: IntEnvironment -> IntExpr -> Integer
evalInt ienv (IVarAdd v k) = ienv Map.! v + k
evalInt _ (IConst k) = k

data Expr
  = Variable Var
  | Constant Json
  | IntLang IntExpr
  -- This will also have somethin to look at the event
  deriving (Eq, Show)

data Predicate
  = Eq Expr Expr
  deriving (Eq, Show)

data Formula
  = P Predicate
  | Always Formula
  | Eventually Formula
  | ForallNode NodeVar Formula
  | ExistsNode NodeVar Formula
  | ForallInt [Integer] IntVar Formula
  | ExistsInt [Integer] IntVar Formula
  | Imp Formula Formula
  | And Formula Formula
  | Or Formula Formula
  | Neg Formula
  | TT
  | FF
  deriving (Eq, Show)

concreteN :: [Node] -> NodeExpression -> NodeExpression
concreteN nodes n@(ConcreteNode{}) = n
concreteN nodes (VariableNode n)
  | n `elem` nodes = ConcreteNode n
  | otherwise = VariableNode n

concreteE :: [Node] -> Expr -> Expr
concreteE nodes (Variable (Var t n jq)) = Variable (Var t (concreteN nodes n) jq)
concreteE nodes j@(Constant{}) = j
concreteE nodes i@(IntLang{}) = i

concreteP :: [Node] -> Predicate -> Predicate
concreteP nodes (Eq l r) = Eq (concreteE nodes l) (concreteE nodes r)

concreteNodes :: [Node] -> Formula -> Formula
concreteNodes nodes form = case form of
  TT -> TT
  FF -> FF
  Neg f -> Neg $ go f
  Or l r -> Or (go l) (go r)
  And l r -> And (go l) (go r)
  Imp l r -> Imp (go l) (go r)
  ExistsInt vs v f -> ExistsInt vs v (go f)
  ForallInt vs v f -> ForallInt vs v (go f)
  ExistsNode n f -> ExistsNode n (concreteNodes (nodes \\ [n]) f)
  ForallNode n f -> ForallNode n (concreteNodes (nodes \\ [n]) f)
  Eventually f -> Eventually (go f)
  Always f -> Always (go f)
  P p -> P $ concreteP nodes p
  where
    go = concreteNodes nodes
