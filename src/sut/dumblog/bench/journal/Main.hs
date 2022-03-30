{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Control.Exception (catch, IOException)
import System.Directory (removePathForcibly)
import System.Posix.SharedMem (shmUnlink)

import Dumblog.Common.Constants
import Dumblog.Journal.Main

import Common

------------------------------------------------------------------------

main :: IO ()
main = do
  shmUnlink (dumblogJournalPath dUMBLOG_PORT)
    `catch` (\(_ :: IOException) -> return ())
  removePathForcibly (dumblogSnapshotPath dUMBLOG_PORT)
  commonMain "Journal" (journalDumblog quietRun bUFFER_CAPACITY . Just)
