module Main where

import Dumblog.SQLite.Main (sqliteDumblog)

import Common

------------------------------------------------------------------------

main :: IO ()
main = commonMain "SQLite" (sqliteDumblog bUFFER_CAPACITY pORT . Just)
