module Main where

import qualified Driver
import Test.Tasty

------------------------------------------------------------------------

main :: IO ()
main = defaultMain =<< Driver.tests
