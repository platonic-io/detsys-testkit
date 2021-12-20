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

import Journal.Internal.ByteBuffer

------------------------------------------------------------------------

data Metrics a = Metrics
  { mMetadata :: ByteBuffer
  , mBuffer   :: ByteBuffer
  }

newtype MetricsSchema a = MetricsSchema
  { unMetricsSchema :: [(a, MetricsType)]
  }

data MetricsType = Counter | Histogram

newMetrics :: (Enum a, Bounded a) => MetricsSchema a -> FilePath -> IO (Metrics a)
newMetrics ms@(MetricsSchema xs) fp = do
  bb <- mmapped fp (sizeOfOffsets + sizeOfMetrics ms)
  meta <- wrapPart bb 0 sizeOfOffsets
  buf  <- wrapPart bb (sizeOfOffsets + 1) (sizeOfOffsets + sizeOfMetrics ms)
  return (Metrics meta buf)
  where
    sizeOfOffsets = length xs * sizeOf (8 :: Int)

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
lookupOffset m x = undefined -- mOffsets m Vector.! fromEnum x

calculateOffsets :: (Enum a, Bounded a) => MetricsSchema a -> Vector Int
calculateOffsets (MetricsSchema xs)
  = Vector.take (length xs)
  . Vector.fromList
  . scanl (\ih (_, mty) -> intsOfSpaceNeeded mty + ih) 0
  $ xs

intsOfSpaceNeeded :: MetricsType -> Int
intsOfSpaceNeeded Counter = 1 -- count
intsOfSpaceNeeded Histogram
  = 2 ^ 16 -- buckets
  + 1      -- sum
  + 1      -- count

sizeOfMetrics :: MetricsSchema a -> Int
sizeOfMetrics
  = (* sizeOf (8 :: Int))
  . sum
  . map (intsOfSpaceNeeded . snd)
  . unMetricsSchema

addNewCounter :: ByteBuffer -> IO ()
addNewCounter bb = undefined

addNewHistogram :: ByteBuffer -> IO ()
addNewHistogram = undefined

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
