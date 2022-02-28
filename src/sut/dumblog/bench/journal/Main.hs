module Main where

import Dumblog.Journal.Main (journalDumblog)

import Common

------------------------------------------------------------------------

main :: IO ()
main = commonMain "Journal" (journalDumblog bUFFER_CAPACITY pORT . Just)
