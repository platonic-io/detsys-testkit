{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}

module Journal.Internal.Metrics where

import Data.Vector.Unboxed (Vector)
import qualified Data.Vector.Unboxed as Vector
import Data.Word
import Foreign
import GHC.Exts
import GHC.ForeignPtr
import GHC.Prim
import GHC.Types

------------------------------------------------------------------------

data Metrics a = Metrics
  { mPtr     :: ForeignPtr Word8
  , mOffsets :: Vector Int
  }

newtype MetricsSchema a = MetricsSchema
  { unMetricsSchema :: [(a, MetricsType)]
  }

data MetricsType = Counter | Histogram

newMetrics :: (Enum a, Bounded a) => MetricsSchema a -> FilePath -> IO (Metrics a)
newMetrics ms@(MetricsSchema xs) fp = IO $ \s ->
  -- XXX: just use mallocForeignPtrBytes :: Int -> IO (ForeignPtr a)
  -- XXX: Aligned to avoid possible false sharing?
  case newPinnedByteArray# len s of
    (# s', arr #) -> case byteArrayContents# (unsafeCoerce# arr) of
      addr# -> (# s', Metrics (ForeignPtr addr# (PlainPtr arr)) (calculateOffsets ms) #)
  -- ptr' <- mmap ptr ...
  -- assertM (ptr' == ptr)
  where
    I# len = length xs

-- XXX: how can we avoid `incrCounter` being called on a histogram?
incrCounter :: (Enum a, Bounded a) => Metrics a -> a -> Int -> IO ()
incrCounter = undefined

measure :: (Enum a, Bounded a) => Metrics a -> a -> Int -> IO ()
measure = undefined

------------------------------------------------------------------------

-- * Internal

data SchemaError = Duplicate

validSchema :: (Enum a, Bounded a) => MetricsSchema a -> Either SchemaError ()
validSchema = undefined -- map (fromEnum . fst) . unMetricsSchema

lookupOffset :: (Enum a, Bounded a) => Metrics a -> a -> Int
lookupOffset m x = mOffsets m Vector.! fromEnum x

calculateOffsets :: (Enum a, Bounded a) => MetricsSchema a -> Vector Int
calculateOffsets (MetricsSchema xs)
  = Vector.take (length xs)
  . Vector.fromList
  . scanl (\ih (_, mty) -> intsOfSpaceNeeded mty + ih) 0
  $ xs
  where
    intsOfSpaceNeeded :: MetricsType -> Int
    intsOfSpaceNeeded Counter = 1 -- count
    intsOfSpaceNeeded Histogram
      = 2 ^ 16 -- buckets
      + 1      -- sum
      + 1      -- count

------------------------------------------------------------------------

-- * Example

data MyMetrics = Connections | Latency
  deriving (Enum, Bounded)

mySchema :: MetricsSchema MyMetrics
mySchema = MetricsSchema [(Connections, Counter), (Latency, Histogram)]

main :: IO ()
main = do
  metrics <- newMetrics mySchema "/tmp/test-metrics"
  incrCounter metrics Connections 1
  measure metrics Latency 200
  return ()
