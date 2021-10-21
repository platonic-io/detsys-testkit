module Disruptor.RingBuffer.MultiProducer where

import Data.IORef (IORef, newIORef)
import Data.Int (Int64)
import Data.Vector.Mutable (IOVector)
import qualified Data.Vector.Mutable as Vector

import Disruptor.SequenceNumber

------------------------------------------------------------------------

-- | The lock-free multi-producer implementation is presented in the following
-- talk:
--
--   https://youtu.be/VBnLW9mKMh4?t=1813
--
-- and also discussed in the following thread:
--
--   https://groups.google.com/g/lmax-disruptor/c/UhmRuz_CL6E/m/-hVt86bHvf8J
--
-- Note that one can also achieve a similar result by using multiple
-- single-producers and combine them into one as outlined in this thread:
--
-- https://groups.google.com/g/lmax-disruptor/c/hvJVE-h2Xu0/m/mBW0j_3SrmIJ
--
data RingBuffer e = RingBuffer
  {
  -- | The capacity, or maximum amount of values, of the ring buffer.
    rbCapacity :: {-# UNPACK #-} !Int64
  -- | The cursor pointing to the head of the ring buffer.
  , rbSequenceNumber :: {-# UNPACK #-} !(IORef SequenceNumber)
  -- | The values of the ring buffer.
  , rbEvents :: {-# UNPACK #-} !(IOVector e)
  -- | References to the last consumers' sequence numbers, used in order to
  -- avoid wrapping the buffer and overwriting events that have not been
  -- consumed yet.
  , rbGatingSequences :: {-# UNPACK #-} !(IORef [IORef SequenceNumber])
  -- | Cached value of computing the last consumers' sequence numbers using the
  -- above references.
  , rbCachedGatingSequence :: {-# UNPACK #-} !(IORef SequenceNumber)
  -- | Used to keep track of what has been published in the multi-producer case.
  , rbAvailableBuffer :: {-# UNPACK #-} !(IOVector Int)
  }

setAvailable :: RingBuffer e -> SequenceNumber -> IO ()
setAvailable rb snr = Vector.write
  (rbAvailableBuffer rb)
  (index (rbCapacity rb) snr)
  (availabilityFlag (rbCapacity rb) snr)
{-# INLINE setAvailable #-}

getAvailable :: RingBuffer e -> Int -> IO Int
getAvailable rb ix = Vector.read (rbAvailableBuffer rb) ix
{-# INLINE getAvailable #-}
