{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE CApiFFI #-}

module Journal.Internal.Atomics where

import Data.Coerce (coerce)
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
{-# INLINE fetchAddWord8Ptr #-}

fetchAddWord16Ptr :: Ptr Word16 -> Word16 -> IO Word16
fetchAddWord16Ptr = c_atomic_fetch_add_word_2
{-# INLINE fetchAddWord16Ptr #-}

fetchAddWord32Ptr :: Ptr Word32 -> Word32 -> IO Word32
fetchAddWord32Ptr = c_atomic_fetch_add_word_4
{-# INLINE fetchAddWord32Ptr #-}

fetchAddWord64Ptr :: Ptr Word64 -> Word64 -> IO Word64
fetchAddWord64Ptr = c_atomic_fetch_add_word_8
{-# INLINE fetchAddWord64Ptr #-}

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
{-# INLINE fetchAddInt8Ptr #-}

fetchAddInt16Ptr :: Ptr Int16 -> Int16 -> IO Int16
fetchAddInt16Ptr = c_atomic_fetch_add_int_2
{-# INLINE fetchAddInt16Ptr #-}

fetchAddInt32Ptr :: Ptr Int32 -> Int32 -> IO Int32
fetchAddInt32Ptr = c_atomic_fetch_add_int_4
{-# INLINE fetchAddInt32Ptr #-}

fetchAddInt64Ptr :: Ptr Int64 -> Int64 -> IO Int64
fetchAddInt64Ptr = c_atomic_fetch_add_int_8
{-# INLINE fetchAddInt64Ptr #-}

------------------------------------------------------------------------

foreign import ccall unsafe "c_atomic_compare_exchange_strong"
  c_atomic_compare_exchange_strong_4 :: Ptr Int32 -> Int32 -> Int32 -> IO CBool

newtype {-# CTYPE "atomic_llong" #-} AtomicLong = AtomicLong Int64

foreign import capi "stdatomic.h atomic_compare_exchange_strong"
  c_atomic_compare_exchange_strong_8 :: Ptr AtomicLong -> Ptr Int64 -> Int64 -> IO CBool


casInt32Ptr :: Ptr Int32 -> Int32 -> Int32 -> IO Bool
casInt32Ptr ptr expected desired = do
  result <- c_atomic_compare_exchange_strong_4 ptr expected desired
  case result of
    0 -> return False
    1 -> return True
    _ ->
      error "casInt32Addr: impossible, c_atomic_compare_exchange_strong should return a _Bool"

casInt64Ptr :: Ptr Int64 -> Int64 -> Int64 -> IO Bool
casInt64Ptr ptr expected desired = alloca $ \ expected_ptr -> do
  poke expected_ptr expected
  result <- c_atomic_compare_exchange_strong_8 (coerce ptr) expected_ptr desired
  case result of
    0 -> return False
    1 -> return True
    _ ->
      error "casInt64Addr: impossible, c_atomic_compare_exchange_strong should return a _Bool"

casIntPtr :: Ptr Int -> Int -> Int -> IO Bool
casIntPtr ptr expected desired =
  casInt64Ptr (castPtr ptr) (int2Int64 expected) (int2Int64 desired)
