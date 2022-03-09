module Main where

import Dumblog.Journal.Main (DumblogConfig(Run), journalDumblog)

import Common

------------------------------------------------------------------------

main :: IO ()
main = commonMain "Journal" (journalDumblog Run bUFFER_CAPACITY pORT . Just)
