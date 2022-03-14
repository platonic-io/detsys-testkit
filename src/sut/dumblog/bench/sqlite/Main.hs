module Main where

import System.Directory (removePathForcibly)

import Dumblog.Journal.Main (dUMBLOG_METRICS)
import Dumblog.SQLite.DB (sqlitePath)
import Dumblog.SQLite.Main (sqliteDumblog)

import Common

------------------------------------------------------------------------

main :: IO ()
main = do
  removePathForcibly sqlitePath
  removePathForcibly (sqlitePath ++ "-shm")
  removePathForcibly (sqlitePath ++ "-wal")
  removePathForcibly dUMBLOG_METRICS
  commonMain "SQLite" (sqliteDumblog bUFFER_CAPACITY pORT . Just)
