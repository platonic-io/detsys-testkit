module Journal.Internal.BufferClaim where

import Data.ByteString (ByteString)
import Data.Word (Word8)
import Foreign.ForeignPtr (ForeignPtr, plusForeignPtr, withForeignPtr)
import Foreign.Ptr (Ptr)

import Journal.Internal.ByteBufferPtr
import Journal.Types
import Journal.Internal.Utils

------------------------------------------------------------------------

newtype BufferClaim = BufferClaim ByteBuffer

newBufferClaim :: ByteBuffer -> TermOffset -> Int -> IO BufferClaim
newBufferClaim src (TermOffset offset) len = BufferClaim <$>
  wrapPart src (int322Int offset) len

putBS :: BufferClaim -> ByteString -> IO ()
putBS (BufferClaim bb) bs = putByteString bb bs

withPtr :: BufferClaim -> (Ptr Word8 -> IO a) -> IO a
withPtr (BufferClaim bb) k = do
  Position offset <- readPosition bb
  withForeignPtr (bbPtr bb `plusForeignPtr` offset) k

commit :: BufferClaim -> IO ()
commit (BufferClaim bb) = do
  Position offset <- readPosition bb
  let Capacity frameLen = getCapacity bb
  writeFrameLength bb (fromIntegral offset) (fromIntegral frameLen)

abort :: BufferClaim -> IO ()
abort (BufferClaim bb) = do
  Position offset <- readPosition bb
  let Capacity frameLen = getCapacity bb
  writeFrameType bb (fromIntegral offset) Padding
  writeFrameLength bb (fromIntegral offset) (fromIntegral frameLen)
