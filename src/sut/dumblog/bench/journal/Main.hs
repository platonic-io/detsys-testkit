module Main where

import System.Directory (removePathForcibly)

import Dumblog.Journal.Main

import Common

------------------------------------------------------------------------

main :: IO ()
main = do
  removePathForcibly dUMBLOG_JOURNAL
  removePathForcibly dUMBLOG_SNAPSHOT
  commonMain "Journal" (journalDumblog Run bUFFER_CAPACITY pORT . Just)
