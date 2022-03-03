module Main where

import Dumblog.ZeroCopy.Main (zeroCopyDumblog)

import Common

------------------------------------------------------------------------

main :: IO ()
main = commonMain "ZeroCopy" (zeroCopyDumblog bUFFER_CAPACITY pORT . Just)
