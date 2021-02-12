module Ldfi.Sat where

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Z3.Monad

import Ldfi.Prop

------------------------------------------------------------------------
-- * SAT formula

type Env = Map String AST

toSatAst :: MonadZ3 z3 => Formula -> z3 AST
toSatAst f = do
  let vs = Set.toList (getVars f)
  vs' <- mapM mkFreshBoolVar vs
  let env = Map.fromList (zip vs vs')
  translate env f

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

solve :: MonadZ3 z3 => z3 AST -> z3 (Result, Maybe Model, Maybe String)
solve m = do
  ast <- m
  assert ast
  (result, mModel) <- getModel
  mModelString <- traverse modelToString mModel
  return (result, mModel, mModelString)

sat :: Formula -> IO (Result, Maybe Model, Maybe String)
sat = evalZ3 . solve . toSatAst

satPrint :: Formula -> IO ()
satPrint f = do
  (result, _mModel, mModelString) <- sat f
  print result
  case mModelString of
    Nothing -> return ()
    Just modelString -> putStrLn modelString
