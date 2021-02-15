{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.Text.IO as T
import Options.Generic

import qualified Ldfi
import Ldfi.FailureSpec (FailureSpec(FailureSpec))
import qualified Ldfi.GitHash as Git
import Ldfi.Sat (z3Solver)
import Ldfi.Solver
import Ldfi.Storage

------------------------------------------------------------------------

data Config = Config
  { testId              :: Maybe Int
  , endOfFiniteFailures :: Maybe Int
  , maxCrashes          :: Maybe Int
  , endOfTime           :: Maybe Int
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
        mFailSpec = makeFailureSpec (endOfFiniteFailures cfg) (maxCrashes cfg) (endOfTime cfg)
      in
        case (testId cfg, mFailSpec) of
          (Just tid, Just failSpec) -> do
            sol <- Ldfi.run sqliteStorage z3Solver tid failSpec
            T.putStrLn (marshal sol)
          (_, _) -> help

makeFailureSpec :: Maybe Int -> Maybe Int -> Maybe Int -> Maybe FailureSpec
makeFailureSpec (Just eof) (Just crashes) (Just eot) =
  Just (FailureSpec (toEnum eof) (toEnum crashes) (toEnum eot))
makeFailureSpec _ _ _ = Nothing
