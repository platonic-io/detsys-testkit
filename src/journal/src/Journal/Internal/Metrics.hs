{-# LANGUAGE MagicHash #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UnboxedTuples #-}

module Journal.Internal.Metrics where

import Control.Exception (assert)
import Control.Monad (forM_, replicateM_, void, when)
import Data.Vector.Unboxed (Vector)
import qualified Data.Vector.Unboxed as Vector
import Data.Word
import Foreign (sizeOf)
import GHC.Exts
import GHC.Float (int2Double)
import GHC.ForeignPtr
import System.Directory (doesFileExist)

import Journal.Internal.ByteBufferPtr
import Journal.Internal.Utils (int2Int64, fallocate)

------------------------------------------------------------------------

data Metrics c h = Metrics
  { mCounterBuffer   :: ByteBuffer
  , mHistogramBuffer :: ByteBuffer
  }

data MetricsSchema c h = MetricsSchema
  { msVersion :: Int
  }

metricSizeOfCounters :: forall c h. (Enum c, Bounded c) => MetricsSchema c h -> Int
metricSizeOfCounters _ = (fromEnum (maxBound :: c) + 1) * sizeOfACounter

metricSizeOfHistograms :: forall c h. (Enum h, Bounded h) => MetricsSchema c h -> Int
metricSizeOfHistograms _ = (fromEnum (maxBound :: h) + 1) * sizeOfAHistogram

metricSize :: forall c h. (Enum c, Bounded c, Enum h, Bounded h) => MetricsSchema c h -> Int
metricSize schema = metricSizeOfCounters schema + metricSizeOfHistograms schema

-- TODO have a header in the file and check that schema is the same as existing in file?
newMetrics :: (Enum c, Bounded c, Enum h, Bounded h)
           => MetricsSchema c h -> FilePath -> IO (Metrics c h)
newMetrics schema fp = do
  exists <- doesFileExist fp
  when (not exists) $ do
    -- XXX: should this be page aligned?
    fallocate fp (sizeOfCounters + sizeOfHistograms)
  bb <- mmapped fp (metricSize schema)
  cbuf <- wrapPart bb 0 sizeOfCounters
  hbuf <- wrapPart bb sizeOfCounters sizeOfHistograms
  return (Metrics cbuf hbuf)
  where
    sizeOfCounters = metricSizeOfCounters schema
    sizeOfHistograms = metricSizeOfHistograms schema

cleanMetrics :: Metrics c h -> IO ()
cleanMetrics (Metrics cbuf hbuf) = do
  clean cbuf
  clean hbuf

incrCounter :: (Enum c) => Metrics c h -> c -> Int -> IO ()
incrCounter (Metrics cbuf _) label value = do
  fetchAddInt64Array_ cbuf offset (int2Int64 value)
  where
    offset = sizeOfACounter * fromEnum label

decrCounter_ :: (Enum c) => Metrics c h -> c -> Int -> IO ()
decrCounter_ metrics label i = incrCounter metrics label (-i)

getCounter :: (Enum c) => Metrics c h -> c -> IO Int
getCounter (Metrics cbuf _) label = do
  readIntOffArrayIx cbuf offset
  where
    offset = sizeOfACounter * fromEnum label

measure :: (Enum h) => Metrics c h -> h -> Double -> IO ()
measure (Metrics _ hbuf) label value = do
  let
    offsetToHistogram = sizeOfAHistogram * fromEnum label
    offsetToHistogramSum = offsetToHistogram
    offsetToHistogramCount = offsetToHistogram + sizeOf (8 :: Int)
    offsetToBucket = offsetToHistogram +
      2 * sizeOf (8 :: Int) +
      compress value * sizeOf (8 :: Int)
  fetchAddInt64Array_ hbuf offsetToHistogramSum (round value)
  fetchAddInt64Array_ hbuf offsetToHistogramCount 1
  fetchAddInt64Array_ hbuf offsetToBucket 1

------------------------------------------------------------------------

pRECISION :: Double
pRECISION = 100.0

pRECISION' :: Double
pRECISION' = 1 / pRECISION

compress :: Double -> Int
compress v =
  assert (fromIntegral (fromEnum (pRECISION * log (1.0 + abs v) + 0.5))
           <= realToFrac (maxBound :: Word16)) $
  fromEnum (pRECISION * log (1.0 + abs v) + 0.5)

decompress :: Int -> Double
decompress i = exp (int2Double i * pRECISION') - 1
-- * Internal

-- In bytes
sizeOfACounter :: Int
sizeOfACounter = sizeOf (8 :: Int)

-- In bytes
sizeOfAHistogram :: Int
sizeOfAHistogram
  = (* sizeOf (8 :: Int))
  $ 2 ^ 16 -- buckets
  + 1      -- sum
  + 1      -- count

------------------------------------------------------------------------

percentile :: Enum h => Metrics c h -> h -> Double -> IO (Maybe Double)
percentile (Metrics _ hbuf) label p
  | p > 100.0 = error "percentile: percentiles cannot be over 100"
  | otherwise  = do
      let
        offsetHistogram = sizeOfAHistogram * fromEnum label
        offsetHistogramCount = offsetHistogram + sizeOf (8 :: Int)

        offsetBucket = offsetHistogram + 2 * sizeOf (8 :: Int)
      count <- readIntOffArrayIx hbuf offsetHistogramCount
      if count == 0
      then return Nothing
      else do
        let d = realToFrac count * (p * 0.01)
            target :: Double
            target | d == 0.0  = 1.0
                   | otherwise = d
        go offsetBucket target
      where
        len = 2 ^ 16

        go :: Int -> Double -> IO (Maybe Double)
        go offsetBucket target = go' 0 0.0
          where
            go' :: Int -> Double -> IO (Maybe Double)
            go' idx acc
              | idx >= len  = return Nothing
              | idx < len = do
                  v <- readIntOffArrayIx hbuf (idx * sizeOf (8 :: Int) + offsetBucket)
                  let sum' = realToFrac v + acc
                  if sum' >= target
                  then return (Just (decompress idx))
                  else go' (succ idx) sum'

count :: Enum h => Metrics c h -> h -> IO Int
count (Metrics _cbuf hbuf) label = do
  let offsetHistogram = sizeOfAHistogram * fromEnum label
      offsetHistogramCount = offsetHistogram + sizeOf (8 :: Int)
  readIntOffArrayIx hbuf offsetHistogramCount

-- * Example

data MyMetricsCounter = Connections
  deriving (Enum, Bounded)

data MyMetricsHistogram = Latency
  deriving (Enum, Bounded)

mySchema :: MetricsSchema MyMetricsCounter MyMetricsHistogram
mySchema = MetricsSchema 1

main :: IO ()
main = do
  metrics <- newMetrics mySchema "/tmp/test-metrics"
  cleanMetrics metrics
  incrCounter metrics Connections 1

  addMeasure metrics 9000 20
  addMeasure metrics 900  35
  addMeasure metrics 90   45
  addMeasure metrics 9    50
  addMeasure metrics 1    100

  putStrLn "Checking percentile"
  checkPercentile metrics 0.0 20
  checkPercentile metrics 99.0 35
  checkPercentile metrics 99.89 45
  checkPercentile metrics 99.91 50
  checkPercentile metrics 99.99 50
  checkPercentile metrics 100 100
  where
    addMeasure metrics num q = do
      putStrLn $ "Adding " <> show num <> " measures of " <> show q <> " as Latency"
      replicateM_ num $ measure metrics Latency q
    checkPercentile metrics p ans = do
      m <- percentile metrics Latency p
      putStrLn $ case m of
        Nothing -> "We didn't get a percentile for " <> show p <> " we were expecting " <> show ans
        Just d
          | round d == ans -> "[OK] Percentile " <> show p <> " is " <> show ans
          | otherwise -> "[FAIL] Percentile " <> show p <> " was " <> show (round d) <> " (expected " <> show ans <> ")"
