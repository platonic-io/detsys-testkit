module Main where

import Control.Concurrent

main :: IO ()
main = print =<< getNumCapabilities
