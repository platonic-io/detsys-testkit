module StuntDouble.HistogramTest where

import Control.Monad
import Test.QuickCheck
import Test.HUnit

import StuntDouble.Histogram

------------------------------------------------------------------------

prop_roundtrip :: Double -> Bool
prop_roundtrip d =
  let
    d' = decompress (compress d)
    ad = abs d
  in
    if ad <= 0.25 then True else
    ad * 0.97 <= d' && d' <= ad * 1.03

assertIO :: (Eq a, Show a) => IO a -> a -> Assertion
assertIO io y = do
  x <- io
  x @?= y

unit_measure :: Assertion
unit_measure = do
  h <- newHistogram
  assertIO (measure 1 h) 1
  assertIO (measure 1 h) 2
  assertIO (measure 2 h) 1
  assertIO (measure 2 h) 2
  assertIO (measure 3 h) 1
  assertIO (fmap round <$> percentile 0.0   h) (Just 1)
  assertIO (fmap round <$> percentile 40.0  h) (Just 1)
  assertIO (fmap round <$> percentile 40.1  h) (Just 2)
  assertIO (fmap round <$> percentile 80.0  h) (Just 2)
  assertIO (fmap round <$> percentile 80.1  h) (Just 3)
  assertIO (fmap round <$> percentile 100.0 h) (Just 3)

unit_measureBig :: Assertion
unit_measureBig = do
  h <- newHistogram

  replicateM_ 9000 (measure 20 h)
  replicateM_ 900  (measure 35 h)
  replicateM_ 90   (measure 45 h)
  replicateM_ 9    (measure 50 h)
  measure 100 h

  assertIO (fmap round <$> percentile 0.0   h) (Just 20)
  assertIO (fmap round <$> percentile 99.0  h) (Just 35)
  assertIO (fmap round <$> percentile 99.89 h) (Just 45)
  assertIO (fmap round <$> percentile 99.91 h) (Just 50)
  assertIO (fmap round <$> percentile 99.99 h) (Just 50)
  assertIO (fmap round <$> percentile 100.0 h) (Just 100)
