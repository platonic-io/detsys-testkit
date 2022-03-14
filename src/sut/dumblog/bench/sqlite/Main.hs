module Main where

import System.Directory (removePathForcibly)

import Dumblog.SQLite.Main (sqliteDumblog)
import Dumblog.SQLite.DB (sqlitePath)

import Common

------------------------------------------------------------------------

main :: IO ()
main = do
  removePathForcibly sqlitePath
  removePathForcibly (sqlitePath ++ "-shm")
  removePathForcibly (sqlitePath ++ "-wal")
  commonMain "SQLite" (sqliteDumblog bUFFER_CAPACITY pORT . Just)
