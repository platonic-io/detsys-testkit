module Main where

import Dumblog.Journal.Main

------------------------------------------------------------------------

main :: IO ()
main = journalDumblog (64 * 1024) 8054 Nothing
