{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}

-- Inspired by:
--   https://github.com/jberryman/unagi-chan/blob/master/src/Data/Atomics/Counter/Fat.hs
-- and:
--   https://hackage.haskell.org/package/atomic-primops-0.8.4/docs/src/Data.Atomics.Counter.html

module StuntDouble.AtomicCounterPadded
    ( AtomicCounter()
    , newCounter
    , incrCounter
    , readCounter
    ) where

import Control.Monad.Primitive (RealWorld)
import Data.Primitive.MachDeps (sIZEOF_INT)
import GHC.Exts
import GHC.Types

------------------------------------------------------------------------

data AtomicCounter = AtomicCounter (MutableByteArray# RealWorld)

sIZEOF_CACHELINE :: Int
sIZEOF_CACHELINE = 64
{-# INLINE sIZEOF_CACHELINE #-}

-- | Create a new atomic counter padded with 64-bytes (an x86 cache line) on
-- either side to try to avoid false sharing.
newCounter :: Int -> IO AtomicCounter
newCounter (I# n) = IO $ \realWorld ->
  let I# sz = sIZEOF_CACHELINE in
  case newAlignedPinnedByteArray# sz sz realWorld of
    (# realWorld', arr #) -> case writeIntArray# arr 0# n realWorld' of
      realWorld'' -> (# realWorld'', AtomicCounter arr #)
{-# INLINE newCounter #-}

incrCounter :: Int -> AtomicCounter -> IO Int
incrCounter (I# incr) (AtomicCounter arr) =
  IO (\realWorld -> case fetchAddIntArray# arr 0# incr realWorld of
                      (# realWorld', i #) -> (# realWorld', I# (i +# incr) #))
{-# INLINE incrCounter #-}

readCounter :: AtomicCounter -> IO Int
readCounter (AtomicCounter arr) =
  IO (\realWorld -> case readIntArray# arr 0# realWorld of
                      (# realWorld', i #) -> (# realWorld', I# i #))
{-# INLINE readCounter #-}
