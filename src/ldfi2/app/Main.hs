{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Options.Generic

import qualified Ldfi.GitHash as Git

------------------------------------------------------------------------

data Config = Config
  { testId              :: Maybe Int
  , endOfFiniteFailures :: Maybe Int
  , maxCrashes          :: Maybe Int
  , version             :: Bool
  }
  deriving (Generic, Show)

instance ParseRecord Config

main :: IO ()
main = do
  cfg <- getRecord "Lineage-driven fault injection"
  run cfg

run :: Config -> IO ()
run cfg
  | version cfg = putStrLn Git.version
  | otherwise   = return ()
