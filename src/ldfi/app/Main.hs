{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import qualified Data.Text.IO as T
import qualified Ldfi
import Ldfi.FailureSpec (FailureSpec (FailureSpec))
import Ldfi.Sat (z3Solver)
import Ldfi.Storage
import Options.Generic

------------------------------------------------------------------------

-- When building with Bazel we generate a module containing the git commit hash
-- at compile-time.
#ifdef __BAZEL_BUILD__
import Ldfi.GitHash
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

type FailedRunHelpText = "Mark a RunId as having already failed in the test, the failures used in that run will not be generated again. This option can be repeated to mark several runs"

data Config = Config
  { testId :: Maybe Int <?> "Which TestId to test for",
    failedRunId :: [Int] <?> FailedRunHelpText,
    endOfFiniteFailures :: Maybe Int <?> "The logical time after which we can't generate faults",
    maxCrashes :: Maybe Int <?> "The number of crashes we are allowed to generate",
    endOfTime :: Maybe Int <?> "NOT USED",
    limitNumberOfFaults :: Maybe Int <?> "Set a limit to the number of faults generated",
    version :: Bool <?> "Print the git commit this binary was built from"
  }
  deriving (Generic, Show)

instance ParseRecord Config

main :: IO ()
main = do
  (cfg, help) <- getWithHelp "Lineage-driven fault injection"
  go cfg help

go :: Config -> IO () -> IO ()
go cfg help
  | unHelpful $ version cfg = putStrLn gitHash
  | otherwise =
    let mFailSpec = makeFailureSpec (unHelpful $ endOfFiniteFailures cfg) (unHelpful $ maxCrashes cfg) (unHelpful $ endOfTime cfg) (unHelpful $ limitNumberOfFaults cfg)
     in case (unHelpful $ testId cfg, mFailSpec) of
          (Just tid, Just failSpec) -> do
            let testInformation = TestInformation tid (unHelpful $ failedRunId cfg)
            json <- Ldfi.run' sqliteStorage z3Solver testInformation failSpec
            T.putStrLn json
          (_, _) -> help

makeFailureSpec :: Maybe Int -> Maybe Int -> Maybe Int -> Maybe Int -> Maybe FailureSpec
makeFailureSpec (Just eof) (Just crashes) (Just eot) limitFailures =
  Just (FailureSpec (toEnum eof) (toEnum crashes) (toEnum eot) limitFailures)
makeFailureSpec _ _ _ _ = Nothing
