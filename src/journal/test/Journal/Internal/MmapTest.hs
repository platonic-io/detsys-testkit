{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}

module Journal.Internal.MmapTest where

import Data.Bits ((.|.))
import GHC.Exts (Int(I#), writeInt32OffAddr#, readInt32OffAddr#, indexInt32OffAddr#)
import GHC.IO (IO(IO))
import GHC.Int (Int32(I32#))
import GHC.Ptr (Ptr(Ptr))
import Test.Tasty.HUnit (Assertion, assertEqual, assertBool)
import System.Posix.IO (handleToFd)
import Foreign

import Journal.Internal.Mmap
import Journal.Internal.Utils

------------------------------------------------------------------------

unit_mmap :: Assertion
unit_mmap = do
  pageSize <- sysconfPageSize
  withTempFile "unit_mmap" $ \fp handle -> do
    fallocate fp pageSize
    fd <- handleToFd handle
    Ptr addr# <- mmap Nothing (fromIntegral pageSize)
                    (pROT_READ .|. pROT_WRITE) mAP_SHARED (Just fd) 0

    -- XXX: The following works, why?
    -- Ptr addr# <- mallocBytes (sizeOf (8 :: Int))

    let offset :: Int
        offset@(I# offset#) = 1023 -- 1025 * sizeOf (4 :: Int32)
    -- XXX: 1023 works, 1024 segfaults... why? Is offset in `sizeOf (4 :: Int32)`
    -- rather than bytes? Like in:
    -- https://hackage.haskell.org/package/primitive-0.7.3.0/docs/Data-Primitive-Ptr.html#g:3
    --
    -- But here's the really weird thing, if we use `1025 * sizeOf (4 :: Int32)`
    -- it works?!?!
    --
    -- Or is it a bytes vs words thing? See
    -- https://gitlab.haskell.org/ghc/ghc/-/blob/master/rts/PrimOps.cmm#L94 and
    -- #define ROUNDUP_BYTES_TO_WDS(n) (((n) + sizeof(W_) - 1) / sizeof(W_))
    --
    -- assertBool "unit_mmap: index out of bounds" (offset < pageSize)

    let i :: Int32
        i@(I32# i#) = minBound

    IO $ \s -> case writeInt32OffAddr# addr# offset# i# s of
                 s' -> (# s', () #)

    j <- IO $ \s -> case readInt32OffAddr# addr# offset# s of
                      (# s', k# #) -> (# s' , fromIntegral (I# k#) #)

    assertEqual "" i j
