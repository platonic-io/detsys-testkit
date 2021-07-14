{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TupleSections #-}

module Ldfi.Sat where

import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Data.Hashable (Hashable)
import qualified Data.Map as Map
import GHC.Stack (HasCallStack)
import Ldfi.Prop
import Ldfi.Solver
import Z3.Monad hiding (Solver)

------------------------------------------------------------------------

-- * SAT formula

type VarConstraint k = (Ord k, Hashable k, Show k)

type Env k = HashMap k AST

liftFun :: (VarConstraint k, MonadZ3 z3) => Env k -> FormulaF k -> FormulaF k -> (AST -> AST -> z3 AST) -> z3 (AST, Env k)
liftFun env l r f = do
  (l', env') <- translate' env l
  (r', env'') <- translate' env' r
  a <- f l' r'
  pure (a, env'')

mapMTs :: Monad m => (s -> a -> m (b, s)) -> s -> [a] -> m ([b], s)
mapMTs _ s [] = pure ([], s)
mapMTs f s (x : xs) = do
  (y, s') <- f s x
  (ys, s'') <- mapMTs f s' xs
  pure (y : ys, s'')

-- Should try to speed up
translateVar :: (VarConstraint k, MonadZ3 z3) => Env k -> k -> z3 (AST, Env k)
translateVar env v = case HashMap.lookup v env of
  Nothing -> do
    v' <- mkFreshBoolVar (show v)
    pure (v', HashMap.insert v v' env)
  Just v' -> pure (v', env)

translate' :: (VarConstraint k, MonadZ3 z3) => Env k -> FormulaF k -> z3 (AST, Env k)
translate' env f0 = case f0 of
  l :&& r -> liftFun env l r (\l' r' -> mkAnd [l', r'])
  l :|| r -> liftFun env l r (\l' r' -> mkOr [l', r'])
  l :+ r -> liftFun env l r mkXor
  And fs -> withEnv mkAnd =<< mapMTs translate' env fs
  Or fs -> withEnv mkOr =<< mapMTs translate' env fs
  Neg f -> withEnv mkNot =<< translate' env f
  l :<-> r -> liftFun env l r mkIff
  l :-> r -> liftFun env l r mkImplies
  TT -> (,env) <$> mkTrue
  FF -> (,env) <$> mkFalse
  Var v -> translateVar env v
  AtMost vs i -> do
    (vs', env') <- mapMTs translateVar env vs
    (,env') <$> mkAtMost vs' i

withEnv :: Monad m => (a -> m b) -> (a, s) -> m (b, s)
withEnv m (x, env) = do
  a <- m x
  pure (a, env)

translate :: (VarConstraint k, MonadZ3 z3) => FormulaF k -> z3 (AST, [k], [AST])
translate f0 = do
  (a, env) <- translate' HashMap.empty f0
  let xs = HashMap.toList env
  return (a, map fst xs, map snd xs)

oldSolve :: MonadZ3 z3 => z3 AST -> z3 Result
oldSolve m = do
  ast <- m
  assert ast
  (result, _mModel) <- getModel
  return result

z3Solve :: (VarConstraint k, HasCallStack) => FormulaF k -> IO (Solution k)
z3Solve f = do
  sols <- z3SolveAll (Just 1) f
  case sols of
    [] -> return NoSolution
    sol : _ -> return sol

z3SolveAll :: (VarConstraint k, HasCallStack) => Maybe Int -> FormulaF k -> IO [Solution k]
z3SolveAll limit f = do
  evalZ3 $ do
    (a, vs, vs') <- translate f
    assert a
    go 0 [] vs vs'
  where
    limitReached :: Int -> Bool
    limitReached n = case limit of
      Nothing -> False
      Just k -> n >= k

    -- should vs be list?
    go :: (VarConstraint k, MonadZ3 z3) => Int -> [Solution k] -> [k] -> [AST] -> z3 [Solution k]
    go n acc vs vs'
      | limitReached n = return (reverse acc)
      | otherwise = do
        (result, mModel) <- getModel -- this takes most time
        case result of
          Undef -> error "impossible"
          Unsat -> return (reverse acc)
          Sat -> case mModel of
            Nothing -> error "impossible" -- TODO(stevan): steal Agda's __IMPOSSIBLE__?
            Just model -> do
              mbs <- mapM (evalBool model) vs'
              let bs = map (maybe (error "impossible") id) mbs
              bs' <-
                mapM
                  (\(v', b) -> mkNot =<< mkEq v' =<< mkBool b)
                  (zip vs' bs)
              -- we should have a comment why we need this assert
              assert =<< mkOr bs'
              go (succ n) (Solution (Map.fromList (zip vs bs)) : acc) vs vs'

z3Solver :: VarConstraint k => Solver k IO
z3Solver = Solver z3Solve
