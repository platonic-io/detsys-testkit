module StuntDouble.RandomTest where

import Data.List
import Test.HUnit
import Test.Tasty.QuickCheck (Property, (===))

import StuntDouble.Random

------------------------------------------------------------------------

unit_shuffle0 :: Assertion
unit_shuffle0 = fst (shuffle (makeSeed 0) [1, 2, 3, 4]) @=? [1, 4, 3, 2]

unit_shuffle1 :: Assertion
unit_shuffle1 = fst (shuffle (makeSeed 1) [1, 2, 3, 4]) @=? [1, 3, 2, 4]

unit_shuffle2 :: Assertion
unit_shuffle2 = fst (shuffle (makeSeed 2) [1, 2, 3, 4]) @=? [3, 2, 4, 1]

unit_shuffleEmpty :: Assertion
unit_shuffleEmpty = fst (shuffle (makeSeed 0) []) @=? ([] :: [Int])

prop_shuffle :: Int -> [Int] -> Property
prop_shuffle i xs = sort (fst (shuffle (makeSeed i) xs)) === sort xs
