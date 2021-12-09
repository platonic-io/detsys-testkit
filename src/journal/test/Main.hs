module Main
  ( module X
  , quickCheck
  , quickCheckWith
  , verboseCheck
  , verboseCheckWith
  , stdArgs
  , maxSuccess
  , main
  ) where

import qualified TastyDiscover
import Test.QuickCheck
       ( maxSuccess
       , quickCheck
       , quickCheckWith
       , stdArgs
       , verboseCheck
       , verboseCheckWith
       )
import Test.Tasty

import Journal as X
import Journal.Internal as X
import JournalTest as X

------------------------------------------------------------------------

main :: IO ()
main = defaultMain =<< TastyDiscover.tests
