-----------------------------------------------------------------------------
-- |
-- Module      :  StuntDouble.Histogram
-- Copyright   :  (C) 2021, Symbiont Inc; (C) 2016, Tyler Neely; (C) 2014, The
--                Cockroach Authors.
-- License     :  Apache-2.0 (see the file LICENSE)
--
-- Maintainer  :  Stevan Andjelkovic <stevan.andjelkovic@symbiont.io>
-- Stability   :  provisional
-- Portability :  non-portable (GHC extensions)
--
-- This module is a Haskell port of Tyler Neely's historian Rust
-- [crate](https://github.com/spacejam/historian), which in turn seems to be
-- derived from his [loghisto](https://github.com/spacejam/loghisto) Golang
-- library. Neely's code bases both use the [Apache license version
-- 2.0](https://www.apache.org/licenses/LICENSE-2.0).
--
-----------------------------------------------------------------------------
module StuntDouble.Histogram where

import Control.Exception
import Data.Coerce
import Data.Word
import Data.Atomics.Counter
import Data.Vector (Vector)
import qualified Data.Vector as Vector

------------------------------------------------------------------------

precision :: Double
precision = 100.0

buckets :: Int
buckets = 2 ^ 16

data Histogram = Histogram
  { histoValues :: Vector AtomicCounter
  , histoSum    :: AtomicCounter
  , histoCount  :: AtomicCounter
  }

newHistogram :: IO Histogram
newHistogram = Histogram
  <$> Vector.replicateM buckets (newCounter 0)
  <*> newCounter 0
  <*> newCounter 0

-- | The value @v@ must be positive. For values larger or equal to @1@ the
-- compression loss is less than @1%@.
measure :: RealFrac a => a -> Histogram -> IO Int
measure v h = do
  incrCounter_ (round v) (histoSum h)
  incrCounter_ 1 (histoCount h)
  incrCounter  1 (histoValues h Vector.! fromEnum (compress v))

compress :: RealFrac a => a -> Word16
compress v =
  let
    d, d' :: Double
    d = realToFrac v

    d' = precision * log (1.0 + abs d) + 0.5
  in
    assert (d' <= realToFrac (maxBound :: Word16))
      (fromIntegral (fromEnum d'))

decompress :: Word16 -> Double
decompress w = exp (realToFrac w / precision) - 1

percentile :: Double -> Histogram -> IO (Maybe Double)
percentile p h
  | p > 100.0 = error "percentile: percentiles cannot be over 100"
  | otherwise  = do
      count <- readCounter (histoCount h)
      if count == 0
      then return Nothing
      else do
        let target :: Double
            target = let
                        d = realToFrac count * (p / 100.0)
                     in
                        if d == 0.0 then 1.0 else d
        go target (histoValues h)
      where
        go :: Double -> Vector AtomicCounter -> IO (Maybe Double)
        go target = go' 0 0.0
          where
            go' :: Word16 -> Double -> Vector AtomicCounter -> IO (Maybe Double)
            go' idx acc xs
              | Vector.null xs = return Nothing
              | otherwise      = do
                  v <- readCounter (Vector.head xs)
                  let sum' = realToFrac v + acc
                  if sum' >= target
                  then return (Just (decompress idx))
                  else go' (succ idx) sum' (Vector.tail xs)

hsum :: Histogram -> IO Int
hsum = readCounter . histoSum

hcount :: Histogram -> IO Int
hcount = readCounter . histoCount
