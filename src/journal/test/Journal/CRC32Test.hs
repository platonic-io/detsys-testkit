module Journal.CRC32Test where

import Data.ByteString (ByteString)
import Data.ByteString.Unsafe (unsafeUseAsCStringLen)
import Data.Word (Word32)
import Test.QuickCheck (Property)
import Test.QuickCheck.Instances.ByteString ()
import Test.QuickCheck.Monadic (run, monadicIO, assert)

import Foreign.C.String (CString)
import Foreign.C.Types (CUInt(CUInt), CULong(CULong))

import Journal.CRC32 (crc32)

------------------------------------------------------------------------

foreign import ccall "zlib.h crc32"
  zlib_crc32 :: CULong -> CString -> CUInt -> CULong

zlibCrc32 :: ByteString -> IO Word32
zlibCrc32 bs = unsafeUseAsCStringLen bs $ \(cstr, len) ->
  return (fromIntegral (zlib_crc32 0 cstr (fromIntegral len)))

prop_crc32 :: ByteString -> Property
prop_crc32 bs = monadicIO $ do
  crc <- run (zlibCrc32 bs)
  assert (crc32 bs == crc)
