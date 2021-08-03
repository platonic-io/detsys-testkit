{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import qualified Description
import Options.Generic
import qualified Stats

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

-- shold we use some other type for FilePath that isn't just alias = [Char]
data Config
  = Check
      { testId :: Int <?> "Which TestId to get stats for",
        file :: FilePath <?> "File that contains the description of the stats"
      }
  | Version
  deriving (Generic)

instance ParseRecord Config

main :: IO ()
main = do
  (cfg, help) <- getWithHelp "Stats"
  case cfg of
    Version -> putStrLn gitHash
    Check {..} -> do
      input <- readFile $ unHelpful file
      case Description.parse input of
        Nothing -> do
          putStrLn "Can't parse <file>"
        Just desc -> do
          (result, nrRuns) <- Stats.gatherInformation (unHelpful testId) desc
          putStrLn $ Description.pprint nrRuns result
