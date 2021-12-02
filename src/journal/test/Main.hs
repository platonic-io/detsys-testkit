module Main
  ( module X
  , quickCheck
  , verboseCheck
  , main
  ) where

import qualified TastyDiscover
import Test.QuickCheck (quickCheck, verboseCheck)
import Test.Tasty

import JournalTest as X

------------------------------------------------------------------------

main :: IO ()
main = defaultMain =<< TastyDiscover.tests
