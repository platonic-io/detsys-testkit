{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Options.Generic

import qualified Ldfi
import Ldfi.FailureSpec (FailureSpec)
import qualified Ldfi.GitHash as Git
import Ldfi.Sat (z3Solver)
import Ldfi.Solver
import Ldfi.Storage

------------------------------------------------------------------------

data Config = Config
  { testId              :: Maybe Int
  , endOfTime           :: Maybe Int
  , endOfFiniteFailures :: Maybe Int
  , maxCrashes          :: Maybe Int
  , version             :: Bool
  }
  deriving (Generic, Show)

instance ParseRecord Config

main :: IO ()
main = do
  (cfg, help) <- getWithHelp "Lineage-driven fault injection"
  go cfg help

go :: Config -> IO () -> IO ()
go cfg help
  | version cfg = putStrLn Git.version
  | otherwise   =
      let
        mFailSpec = makeFailureSpec (testId cfg) (endOfTime cfg) (endOfFiniteFailures cfg)
      in
        case (testId cfg, mFailSpec) of
          (Just tid, Just failSpec) -> do
            sol <- Ldfi.run sqliteStorage z3Solver tid failSpec
            putStrLn (marshal sol)
          (_, _) -> help

makeFailureSpec :: Maybe Int -> Maybe Int -> Maybe Int -> Maybe FailureSpec
makeFailureSpec = undefined

marshal :: Solution -> String
marshal = undefined
