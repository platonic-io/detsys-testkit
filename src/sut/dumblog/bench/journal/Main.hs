module Main where

import System.Directory (removePathForcibly)

import Dumblog.Common.Constants
import Dumblog.Journal.Main

import Common

------------------------------------------------------------------------

main :: IO ()
main = do
  removePathForcibly (dumblogJournalPath dUMBLOG_PORT)
  removePathForcibly (dumblogSnapshotPath dUMBLOG_PORT)
  commonMain "Journal" (journalDumblog quietRun bUFFER_CAPACITY . Just)
