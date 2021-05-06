{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import Divergence
import Options.Generic

------------------------------------------------------------------------

-- When building with Bazel we generate a module containing the git commit hash
-- at compile-time.
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

------------------------------------------------------------------------

data Config
  = Check
      { one :: Int <?> "The first  TestID",
        two :: Int <?> "The second TestID"
      }
  | Version
  deriving (Generic)

instance ParseRecord Config

main :: IO ()
main = do
  (cfg, _help) <- getWithHelp "Divergence checker"
  case cfg of
    Version -> putStrLn gitHash
    Check {..} -> do
      mdiff <- divergence (unHelpful one) (unHelpful two)
      case mdiff of
        Nothing -> putStrLn "The tests are equivalent"
        Just x -> do
          putStrLn $ "The tests are different:"
          putStrLn $ prettyError x
