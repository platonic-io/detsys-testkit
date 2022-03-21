module Journal.Internal.BufferClaim where

import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as LBS
import Data.Int (Int32, Int64)
import Data.Word (Word8)
import Foreign.ForeignPtr (ForeignPtr, plusForeignPtr, withForeignPtr)
import Foreign.Ptr (Ptr)

import Journal.Internal.ByteBufferPtr
import Journal.Internal.Logger (Logger, logg)
import Journal.Internal.Utils
import Journal.Types

------------------------------------------------------------------------

newtype BufferClaim = BufferClaim { bcByteBuffer :: ByteBuffer }

newBufferClaim :: ByteBuffer -> TermOffset -> Int -> IO BufferClaim
newBufferClaim src (TermOffset offset) len = BufferClaim <$>
  wrapPart src (int322Int offset) len

putBS :: BufferClaim -> Int -> ByteString -> IO ()
putBS (BufferClaim bb) offset bs = putByteStringAt bb offset bs

putLBS :: BufferClaim -> Int -> LBS.ByteString -> IO ()
putLBS (BufferClaim bb) offset bs = putLazyByteStringAt bb offset bs

putInt32At :: BufferClaim -> Int -> Int32 -> IO ()
putInt32At (BufferClaim bb) offset i32 = writeInt32OffAddr bb offset i32

putInt64At :: BufferClaim -> Int -> Int64 -> IO ()
putInt64At (BufferClaim bb) offset i64 = writeInt64OffAddr bb offset i64

withPtr :: BufferClaim -> (Ptr Word8 -> IO a) -> IO a
withPtr (BufferClaim bb) k = do
  Slice slice <- readSlice bb
  -- XXX: boundcheck?
  withForeignPtr (bbData bb `plusForeignPtr` slice) k

commit :: BufferClaim -> Logger -> WaitingStrategy -> IO ()
commit (BufferClaim bb) logger rn = do
  let Capacity frameLen = getCapacity bb
  logg logger ("commit, frameLen: " ++ show frameLen)
  writeFrameType bb 0 Valid
  writeFrameLength bb 0 (HeaderLength (int2Int32 frameLen))
  notifyReader rn

abort :: BufferClaim -> IO ()
abort (BufferClaim bb) = do
  let Capacity frameLen = getCapacity bb
  writeFrameType bb 0 Padding
  writeFrameLength bb 0 (HeaderLength (int2Int32 frameLen))
