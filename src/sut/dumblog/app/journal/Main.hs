{-# LANGUAGE OverloadedStrings #-}
module Main where

import Options.Generic
import Dumblog.Journal.Main

------------------------------------------------------------------------

main :: IO ()
main = do
  (cfg, _help) <- getWithHelp "Dumblog (journal)"
  journalDumblog cfg (64 * 1024) 8054 Nothing
