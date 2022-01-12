-----------------------------------------------------------------------------
-- |
-- Module      :  StuntDouble.Histogram.SingleProducer
-- Copyright   :  (C) 2021-2022, Symbiont Inc; (C) 2016, Tyler Neely; (C) 2014, The
--                Cockroach Authors.
-- License     :  MIT and Apache-2.0 (see the file LICENSE)
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
import Data.Vector.Unboxed.Mutable (IOVector)
import qualified Data.Vector.Unboxed.Mutable as Vector
import Data.Word
import Data.Bits
import GHC.Float
import Data.IORef
import Text.Printf

------------------------------------------------------------------------

pRECISION :: Double
pRECISION = 100.0
{-# INLINE pRECISION #-}

pRECISION' :: Double
pRECISION' = 1 / pRECISION
{-# INLINE pRECISION' #-}

bUCKETS :: Int
bUCKETS = 2 ^ 16
{-# INLINE bUCKETS #-}

newtype Histogram = Histogram { unHistogram :: IOVector Word32 }

-- NOTE: The first two elements of the array are reserved for the sum and count,
-- so the values start at position 2.
hISTOGRAM_SUM_INDEX :: Int
hISTOGRAM_SUM_INDEX = 0
{-# INLINE hISTOGRAM_SUM_INDEX #-}

hISTOGRAM_COUNT_INDEX :: Int
hISTOGRAM_COUNT_INDEX = 1
{-# INLINE hISTOGRAM_COUNT_INDEX #-}

hISTOGRAM_VALUES_OFFSET :: Int
hISTOGRAM_VALUES_OFFSET = 2
{-# INLINE hISTOGRAM_VALUES_OFFSET #-}

newHistogram :: IO Histogram
newHistogram =
  Histogram <$> Vector.replicate (bUCKETS + hISTOGRAM_VALUES_OFFSET) 0
{-# INLINE newHistogram #-}

-- | The value @v@ must be positive. For values larger or equal to @1@ the
-- compression loss is less than @1%@.
measure :: Double -> Histogram -> IO Word32
measure v (Histogram h) = do
  Vector.unsafeModify h (+ round v) hISTOGRAM_SUM_INDEX
  Vector.unsafeModify h (+ 1) hISTOGRAM_COUNT_INDEX
  let ix = compress v + hISTOGRAM_VALUES_OFFSET
  count' <- (+ 1) <$> Vector.unsafeRead h ix
  Vector.unsafeWrite h ix count'
  return count'
{-# INLINABLE measure #-}

measure_ :: Double -> Histogram -> IO ()
measure_ v (Histogram h) = do
  Vector.unsafeModify h (+ fromIntegral (double2Int v)) hISTOGRAM_SUM_INDEX
  Vector.unsafeModify h (+ 1) hISTOGRAM_COUNT_INDEX
  Vector.unsafeModify h (+ 1) (compress v + hISTOGRAM_VALUES_OFFSET)
{-# INLINE measure_ #-}

measureInt_ :: Int -> Histogram -> IO ()
measureInt_ v (Histogram h) = do
  Vector.unsafeModify h (+ fromIntegral v) hISTOGRAM_SUM_INDEX
  Vector.unsafeModify h (+ 1) hISTOGRAM_COUNT_INDEX
  Vector.unsafeModify h (+ 1) (compressInt v + hISTOGRAM_VALUES_OFFSET)
{-# INLINE measureInt_ #-}

compress :: Double -> Int
compress v =
  assert (fromIntegral (fromEnum (pRECISION * log (1.0 + abs v) + 0.5))
           <= realToFrac (maxBound :: Word16))
  (fromEnum (pRECISION * log (1.0 + abs v) + 0.5))
{-# INLINE compress #-}

compressInt :: Int -> Int
compressInt v =
  assert (fromIntegral (fromEnum (pRECISION * log (1.0 + int2Double (abs v)) + 0.5))
           <= realToFrac (maxBound :: Word16))
  (fromEnum (pRECISION * log (1.0 + int2Double (abs v)) + 0.5))
{-# INLINE compressInt #-}

decompress :: Int -> Double
decompress i = exp (int2Double i * pRECISION') - 1
{-# INLINE decompress #-}

percentile :: Double -> Histogram -> IO (Maybe Double)
percentile p (Histogram h)
  | p > 100.0 = error "percentile: percentiles cannot be over 100"
  | otherwise  = do
      count <- Vector.unsafeRead h hISTOGRAM_COUNT_INDEX
      if count == 0
      then return Nothing
      else do
        let target :: Double
            target = let
                        d = realToFrac count * (p * 0.01)
                     in
                        if d == 0.0 then 1.0 else d
        go target h
      where
        go :: Double -> IOVector Word32 -> IO (Maybe Double)
        go target xs = go' 0 0.0
          where
            len = fromIntegral (Vector.length xs) - hISTOGRAM_VALUES_OFFSET - 1

            go' :: Int -> Double -> IO (Maybe Double)
            go' idx acc
              | idx > len  = return Nothing
              | idx <= len = do
                  v <- Vector.unsafeRead xs (idx + hISTOGRAM_VALUES_OFFSET)
                  let sum' = realToFrac v + acc
                  if sum' >= target
                  then return (Just (decompress idx))
                  else go' (succ idx) sum'

hsum :: Histogram -> IO Int
hsum
  = fmap fromIntegral
  . flip Vector.unsafeRead hISTOGRAM_SUM_INDEX
  . unHistogram
{-# INLINE hsum #-}

hcount :: Histogram -> IO Int
hcount
  = fmap fromIntegral
  . flip Vector.unsafeRead hISTOGRAM_COUNT_INDEX
  . unHistogram
{-# INLINE hcount #-}

hmean :: Histogram -> IO Double
hmean h = (/) <$> fmap int2Double (hsum h) <*> fmap int2Double (hcount h)

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
