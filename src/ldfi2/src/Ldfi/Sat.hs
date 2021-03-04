module Ldfi.Sat where

import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Set as Set
import GHC.Stack (HasCallStack)
import Ldfi.Prop
import Ldfi.Solver
import Z3.Monad hiding (Solver)

------------------------------------------------------------------------

-- * SAT formula

type Env = Map String AST

liftFun :: MonadZ3 z3 => Env -> Formula -> Formula -> (AST -> AST -> z3 AST) -> z3 AST
liftFun env l r f = do
  l' <- translate env l
  r' <- translate env r
  f l' r'

translate :: MonadZ3 z3 => Env -> Formula -> z3 AST
translate env f0 = case f0 of
  l :&& r -> liftFun env l r (\l' r' -> mkAnd [l', r'])
  l :|| r -> liftFun env l r (\l' r' -> mkOr [l', r'])
  l :+ r -> liftFun env l r mkXor
  And fs -> mkAnd =<< mapM (translate env) fs
  Or fs -> mkOr =<< mapM (translate env) fs
  Neg f -> mkNot =<< translate env f
  l :<-> r -> liftFun env l r mkIff
  l :-> r -> liftFun env l r mkImplies
  TT -> mkTrue
  FF -> mkFalse
  Var v -> return (env Map.! v)
  AtMost vs i -> mkAtMost [env Map.! v | v <- vs] i

oldSolve :: MonadZ3 z3 => z3 AST -> z3 Result
oldSolve m = do
  ast <- m
  assert ast
  (result, _mModel) <- getModel
  return result

z3Solve :: HasCallStack => Formula -> IO Solution
z3Solve f = do
  sols <- z3SolveAll (Just 1) f
  case sols of
    [] -> return NoSolution
    sol : _ -> return sol

z3SolveAll :: HasCallStack => Maybe Int -> Formula -> IO [Solution]
z3SolveAll limit f = do
  let vs = Set.toList (getVars f)
  evalZ3 $ do
    vs' <- mapM mkFreshBoolVar vs
    let env = Map.fromList (zip vs vs')
    a <- translate env f
    assert a
    go 0 [] vs vs'
    where
      limitReached :: Int -> Bool
      limitReached n = case limit of
        Nothing -> False
        Just k -> n >= k

      go :: MonadZ3 z3 => Int -> [Solution] -> [String] -> [AST] -> z3 [Solution]
      go n acc vs vs'
        | limitReached n = return (reverse acc)
        | otherwise = do
          (result, mModel) <- getModel
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
                    (\(ix, b) -> mkNot =<< mkEq (vs' !! ix) =<< mkBool b)
                    (zip [0 .. length bs - 1] bs)
                assert =<< mkOr bs'
                go (succ n) (Solution (Map.fromList (zip vs bs)) : acc) vs vs'

z3Solver :: Solver IO
z3Solver = Solver z3Solve
