-----------------------------------------------------------------------------
-- |
-- Module      :  StuntDouble.Histogram.SingleProducer
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
module StuntDouble.Histogram.SingleProducer where

import Control.Exception
import Data.Vector.Mutable (IOVector)
import qualified Data.Vector.Mutable as Vector
import Data.Word
import Data.Bits
import GHC.Float
import Data.IORef
import Text.Printf

------------------------------------------------------------------------

precision :: Double
precision = 100.0

buckets :: Word32
buckets = 2 `shiftL` 16 -- 2^16

data Histogram = Histogram
  { histoValues :: IOVector Word32
  , histoSum    :: IORef Word32
  , histoCount  :: IORef Word32
  }

newHistogram :: IO Histogram
newHistogram = Histogram
  <$> Vector.replicate (fromIntegral buckets) 0
  <*> newIORef 0
  <*> newIORef 0

-- | The value @v@ must be positive. For values larger or equal to @1@ the
-- compression loss is less than @1%@.
measure :: RealFrac a => a -> Histogram -> IO Int
measure v h = do
  modifyIORef' (histoSum   h) (+ round v)
  modifyIORef' (histoCount h) (+ 1)
  let ix = fromIntegral (compress v)
  count <- Vector.read (histoValues h) ix
  Vector.write (histoValues h) ix (count + 1)
  return (fromIntegral count)

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
      count <- readIORef (histoCount h)
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
        go :: Double -> IOVector Word32 -> IO (Maybe Double)
        go target xs = go' 0 0.0
          where
            arrayLength = fromIntegral (Vector.length xs) - 1

            go' :: Word16 -> Double -> IO (Maybe Double)
            go' idx acc
              | idx > arrayLength  = return Nothing
              | idx <= arrayLength = do
                  v <- Vector.read xs (fromIntegral idx)
                  let sum' = realToFrac v + acc
                  if sum' >= target
                  then return (Just (decompress idx))
                  else go' (succ idx) sum'

hsum :: Histogram -> IO Int
hsum = fmap fromIntegral . readIORef . histoSum

hcount :: Histogram -> IO Int
hcount = fmap fromIntegral . readIORef . histoCount

-- | @hstats@ returns `Nothing` if no @measure@s have been made, otherwise it
-- returns `Just` of a list of the minimum, median, the 90-, 99-, 99.9- and
-- 99.99-th percentile, the maximum, total count, and the sum of all @measure@s.
hstats :: Histogram -> IO (Maybe [Double])
hstats h = do
  c <- hcount h
  if c == 0
  then return Nothing
  else do
    mps <- mapM (\p -> percentile p h) [0, 50, 90, 99, 99.9, 99.99, 100]
    s <- hsum h
    let mps' = mps ++ [Just (int2Double c), Just (int2Double s)]
    return (Just (map (maybe (read "NaN") id) mps'))

prettyPrintHistogram :: String -> Histogram -> IO ()
prettyPrintHistogram name h = do
  putStr name
  putStr ": "
  ms <- hstats h
  case ms of
    Nothing -> putStrLn "NaN"
    Just s  -> do
      putStrLn ""
      printf (concat (replicate 9 "%-10s"))
             "min" "med" "90" "99" "99.9" "99.99" "max" "count" "sum"
      putStrLn ""
      mapM_ (printf "%-10.2f") s
