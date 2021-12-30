module Journal.Internal.BufferClaim where

import Data.ByteString (ByteString)
import Data.Word (Word8)
import Foreign.Ptr (Ptr, plusPtr)

import Journal.Internal.ByteBuffer
import Journal.Types

------------------------------------------------------------------------

newtype BufferClaim = BufferClaim ByteBuffer

newBufferClaim :: ByteBuffer -> TermOffset -> Int -> IO BufferClaim
newBufferClaim src offset len = BufferClaim <$>
  wrapPart src (fromIntegral offset) len

putBytes :: BufferClaim -> ByteString -> IO ()
putBytes (BufferClaim bb) bs = putByteString bb bs

-- NOTE: The underlying @ByteBuffer@ must have been pinned, otherwise we cannot
-- guarantee to get a pointer to it that isn't moved around.
withPtr :: BufferClaim -> (Ptr Word8 -> IO a) -> IO a
withPtr (BufferClaim bb) k = do
  Position offset <- readPosition bb
  k (bbPtr bb `plusPtr` offset)

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
