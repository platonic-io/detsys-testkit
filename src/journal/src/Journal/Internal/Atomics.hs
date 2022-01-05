{-# LANGUAGE ForeignFunctionInterface #-}

module Journal.Internal.Atomics where

import Foreign

------------------------------------------------------------------------

foreign import ccall unsafe "stdatomic.h __atomic_fetch_add_1"
  c_atomic_fetch_add_1 :: Ptr Word8 -> Word8 -> IO Word8

foreign import ccall unsafe "stdatomic.h __atomic_fetch_add_2"
  c_atomic_fetch_add_2 :: Ptr Word16 -> Word16 -> IO Word16

foreign import ccall unsafe "stdatomic.h __atomic_fetch_add_4"
  c_atomic_fetch_add_4 :: Ptr Word32 -> Word32 -> IO Word32

foreign import ccall unsafe "stdatomic.h __atomic_fetch_add_8"
  c_atomic_fetch_add_8 :: Ptr Word64 -> Word64 -> IO Word64

fetchAddWord8Ptr :: Ptr Word8 -> Word8 -> IO Word8
fetchAddWord8Ptr = c_atomic_fetch_add_1

fetchAddWord16Ptr :: Ptr Word16 -> Word16 -> IO Word16
fetchAddWord16Ptr = c_atomic_fetch_add_2

fetchAddWord32Ptr :: Ptr Word32 -> Word32 -> IO Word32
fetchAddWord32Ptr = c_atomic_fetch_add_4

fetchAddWord64Ptr :: Ptr Word64 -> Word64 -> IO Word64
fetchAddWord64Ptr = c_atomic_fetch_add_8
