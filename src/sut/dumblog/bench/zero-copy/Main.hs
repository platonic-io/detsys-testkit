module Main where

import System.Directory (removePathForcibly)

import Dumblog.ZeroCopy.Main (zeroCopyDumblog)

import Common

------------------------------------------------------------------------

main :: IO ()
main = do
  let fp = "/tmp/dumblog-zero-copy.journal"
  removePathForcibly fp
  commonMain "ZeroCopy" (zeroCopyDumblog (512 * 1024 * 1024) pORT fp . Just)
