module StuntDouble.HistogramTest where

import GHC.Float
import Control.Monad
import Test.QuickCheck
import Test.HUnit

import StuntDouble.Histogram

------------------------------------------------------------------------

prop_roundtrip :: Positive Double -> Property
prop_roundtrip (Positive d) = withMaxSuccess 100000 $
  d >= 1 ==>
  classify (1    <= d && d < 10)   "1-9" $
  classify (10   <= d && d < 100)  "10-99" $
  classify (100  <= d && d < 1000) "100-999" $
  classify (1000 <= d)             "1000-.." $
  d * 0.99 <= d' && d' <= d * 1.01
  where
    d' = decompress (compress d)

prop_roundtripLarge :: Large Int -> Property
prop_roundtripLarge (Large i) = withMaxSuccess 100000 $
  d >= 1 ==>
  classify (1     <= d && d < 10)       "1-9" $
  classify (10    <= d && d < 100)      "10-99" $
  classify (100   <= d && d < 1000)     "100-999" $
  classify (1000  <= d && d < 10000)    "1000-9999" $
  classify (10000 <= d && d < 100000)   "10000-99999" $
  classify (100000 <= d && d < 1000000) "100000-999999" $
  classify (1000000 <= d)               "1000000-.." $
  d * 0.99 <= d' && d' <= d * 1.01
  where
    d  = int2Double (abs i)
    d' = decompress (compress d)

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
