module Main where

import System.Directory (removePathForcibly)

import Dumblog.Common.Constants (dUMBLOG_PORT)
import Dumblog.SQLite.Main (sqliteDumblog)
import Dumblog.SQLite.DB (sqlitePath)

import Common

------------------------------------------------------------------------

main :: IO ()
main = do
  removePathForcibly sqlitePath
  removePathForcibly (sqlitePath ++ "-shm")
  removePathForcibly (sqlitePath ++ "-wal")
  commonMain "SQLite" (sqliteDumblog bUFFER_CAPACITY dUMBLOG_PORT . Just)
