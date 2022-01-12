{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}

module Journal.Internal.MmapTest where

import Data.Bits ((.|.))
import Foreign
import GHC.Exts
       ( Int(I#)
       , indexInt32OffAddr#
       , plusAddr#
       , readInt32OffAddr#
       , writeInt32OffAddr#
       )
import GHC.IO (IO(IO))
import GHC.Int (Int32(I32#))
import GHC.Ptr (Ptr(Ptr))
import System.Posix.IO (handleToFd)
import Test.Tasty.HUnit (Assertion, assertBool, assertEqual)

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

    let offset :: Int
        offset@(I# offset#) = pageSize - sizeOf (4 :: Int32)

    assertBool "unit_mmap: index out of bounds" (offset < pageSize)

    let i :: Int32
        i@(I32# i#) = minBound

    IO $ \s -> case writeInt32OffAddr# (addr# `plusAddr#` offset#) 0# i# s of
                 s' -> (# s', () #)

    j <- IO $ \s -> case readInt32OffAddr# (addr# `plusAddr#` offset#) 0# s of
                      (# s', k# #) -> (# s' , fromIntegral (I# k#) #)

    assertEqual "" i j
