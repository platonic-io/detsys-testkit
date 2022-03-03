module Main where

import Dumblog.ZeroCopy.Main (zeroCopyDumblog)

import Common

------------------------------------------------------------------------

main :: IO ()
main = commonMain "ZeroCopy" (zeroCopyDumblog (512 * 1024 * 1024) pORT . Just)
