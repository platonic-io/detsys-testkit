module Ldfi.Sat where

import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Set as Set
import Z3.Monad

import Ldfi.Prop
import Ldfi.Solver

------------------------------------------------------------------------
-- * SAT formula

type Env = Map String AST

translate :: MonadZ3 z3 => Env -> Formula -> z3 AST
translate env f0 = case f0 of
  l :&& r -> do
    l' <- translate env l
    r' <- translate env r
    mkAnd [l', r']
  l :|| r -> do
    l' <- translate env l
    r' <- translate env r
    mkOr [l', r']
  And fs -> do
    fs' <- mapM (translate env) fs
    mkAnd fs'
  Or fs -> do
    fs' <- mapM (translate env) fs
    mkOr fs'
  Neg f -> mkNot =<< translate env f
  TT    -> mkTrue
  FF    -> mkFalse
  Var v -> return (env Map.! v)

oldSolve :: MonadZ3 z3 => z3 AST -> z3 Result
oldSolve m =  do
  ast <- m
  assert ast
  (result, _mModel) <- getModel
  return result

z3Solve :: Formula -> IO Solution
z3Solve f = evalZ3 $ do
  let vs = Set.toList (getVars f)
  vs' <- mapM mkFreshBoolVar vs
  let env = Map.fromList (zip vs vs')
  a <- translate env f
  assert a
  (result, mModel) <- getModel
  case result of
    Undef -> error "impossible"
    Unsat -> return NoSolution
    Sat -> case mModel of
      Nothing -> error "impossible" -- TODO(stevan): steal Agda's __IMPOSSIBLE__?
      Just model -> do
        mbs <- mapM (evalBool model) vs'
        let bs = map (maybe (error "impossible") id) mbs
        return (Solution (Map.fromList (zip vs bs)))
