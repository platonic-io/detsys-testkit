{-# LANGUAGE ForeignFunctionInterface #-}

module Journal.Internal.Atomics where

import Foreign
import Foreign.C.Types

import Journal.Internal.Utils (int2Int64)

------------------------------------------------------------------------

foreign import ccall unsafe "stdatomic.h __atomic_fetch_add_1"
  c_atomic_fetch_add_word_1 :: Ptr Word8 -> Word8 -> IO Word8

foreign import ccall unsafe "stdatomic.h __atomic_fetch_add_2"
  c_atomic_fetch_add_word_2 :: Ptr Word16 -> Word16 -> IO Word16

foreign import ccall unsafe "stdatomic.h __atomic_fetch_add_4"
  c_atomic_fetch_add_word_4 :: Ptr Word32 -> Word32 -> IO Word32

foreign import ccall unsafe "stdatomic.h __atomic_fetch_add_8"
  c_atomic_fetch_add_word_8 :: Ptr Word64 -> Word64 -> IO Word64

fetchAddWord8Ptr :: Ptr Word8 -> Word8 -> IO Word8
fetchAddWord8Ptr = c_atomic_fetch_add_word_1

fetchAddWord16Ptr :: Ptr Word16 -> Word16 -> IO Word16
fetchAddWord16Ptr = c_atomic_fetch_add_word_2

fetchAddWord32Ptr :: Ptr Word32 -> Word32 -> IO Word32
fetchAddWord32Ptr = c_atomic_fetch_add_word_4

fetchAddWord64Ptr :: Ptr Word64 -> Word64 -> IO Word64
fetchAddWord64Ptr = c_atomic_fetch_add_word_8

------------------------------------------------------------------------

foreign import ccall unsafe "stdatomic.h __atomic_fetch_add_1"
  c_atomic_fetch_add_int_1 :: Ptr Int8 -> Int8 -> IO Int8

foreign import ccall unsafe "stdatomic.h __atomic_fetch_add_2"
  c_atomic_fetch_add_int_2 :: Ptr Int16 -> Int16 -> IO Int16

foreign import ccall unsafe "stdatomic.h __atomic_fetch_add_4"
  c_atomic_fetch_add_int_4 :: Ptr Int32 -> Int32 -> IO Int32

foreign import ccall unsafe "stdatomic.h __atomic_fetch_add_8"
  c_atomic_fetch_add_int_8 :: Ptr Int64 -> Int64 -> IO Int64

fetchAddInt8Ptr :: Ptr Int8 -> Int8 -> IO Int8
fetchAddInt8Ptr = c_atomic_fetch_add_int_1

fetchAddInt16Ptr :: Ptr Int16 -> Int16 -> IO Int16
fetchAddInt16Ptr = c_atomic_fetch_add_int_2

fetchAddInt32Ptr :: Ptr Int32 -> Int32 -> IO Int32
fetchAddInt32Ptr = c_atomic_fetch_add_int_4

fetchAddInt64Ptr :: Ptr Int64 -> Int64 -> IO Int64
fetchAddInt64Ptr = c_atomic_fetch_add_int_8

------------------------------------------------------------------------

foreign import ccall unsafe "c_atomic_compare_exchange_strong"
  c_atomic_compare_exchange_strong_4 :: Ptr Int32 -> Int32 -> Int32 -> IO CBool

foreign import ccall unsafe "c_atomic_compare_exchange_strong"
  c_atomic_compare_exchange_strong_8 :: Ptr Int64 -> Int64 -> Int64 -> IO CBool

casInt32Ptr :: Ptr Int32 -> Int32 -> Int32 -> IO Bool
casInt32Ptr ptr expected desired = do
  result <- c_atomic_compare_exchange_strong_4 ptr expected desired
  case result of
    0 -> return False
    1 -> return True
    _ ->
      error "casInt32Addr: impossible, c_atomic_compare_exchange_strong should return a _Bool"

casInt64Ptr :: Ptr Int64 -> Int64 -> Int64 -> IO Bool
casInt64Ptr ptr expected desired = do
  result <- c_atomic_compare_exchange_strong_8 ptr expected desired
  case result of
    0 -> return False
    1 -> return True
    _ ->
      error "casInt64Addr: impossible, c_atomic_compare_exchange_strong should return a _Bool"

casIntPtr :: Ptr Int -> Int -> Int -> IO Bool
casIntPtr ptr expected desired =
  casInt64Ptr (castPtr ptr) (int2Int64 expected) (int2Int64 desired)
