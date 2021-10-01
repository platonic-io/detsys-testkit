{-# LANGUAGE CPP #-}

module Main where

import System.Environment (getArgs)

import qualified Scheduler

------------------------------------------------------------------------

#ifdef __BAZEL_BUILD__
import GitHash
-- When building with cabal we expect the git commit hash to be passed in via
-- CPP flags, i.e. `--ghc-option=-D__GIT_HASH__=\"X\"`.
#elif defined __GIT_HASH__
gitHash :: String
gitHash = __GIT_HASH__
#else
gitHash :: String
gitHash = "unknown"
#endif

main :: IO ()
main = do
  as <- System.Environment.getArgs
  if any (== "--version") as
  then putStrLn gitHash
  else Scheduler.main gitHash
