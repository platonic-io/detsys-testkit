module Main where

import Test.Tasty

import qualified Driver

main :: IO ()
main = defaultMain =<< Driver.tests
