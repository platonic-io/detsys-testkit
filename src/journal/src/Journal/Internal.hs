module Journal.Internal where

import Control.Concurrent.Async
import Data.Binary (encode, decode)
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as LBS
import Data.Word (Word8, Word32)
import Foreign.Ptr (Ptr, plusPtr)
import Foreign.Storable (pokeByteOff, peekByteOff)

import Journal.Types

------------------------------------------------------------------------

-- | The size of the journal entry header in bytes.
hEADER_SIZE :: Int
hEADER_SIZE = 4 -- sizeOf (0 :: Word32)
  -- XXX: version?
  -- XXX: CRC?

claim :: Journal -> Int -> IO Int
claim jour bytes = incrCounter (bytes + hEADER_SIZE) (jOffset jour)

  -- XXX:
  -- if bytes + offset <= jMaxSize then write to active file
  -- if bytes + offset > jMaxSize then
  --    if offset - jMaxSize == bytes then rotate files
  --    e.g. max size = 1000, we are trying to write 100 bytes and the offset we get is 1100

  --    if offset - jMaxSize > bytes then somebody else is responsive for
  --    rotating, we know that's done when jSequence is bumped?

  --    continuing on the above example say we are trying to write 100 bytes we get offset 1200
  -- if bytes + offset > jMaxS

-- XXX: Use Data.Primitive.ByteArray.copyMutableByteArrayToPtr instead?
writeLBSToPtr :: ByteString -> Ptr Word8 -> IO ()
writeLBSToPtr bs ptr | LBS.null bs = return ()
                     | otherwise   = go (fromIntegral (LBS.length bs - 1))
  where
    go :: Int -> IO ()
    go 0 = pokeByteOff ptr 0 (LBS.index bs 0)
    go n = do
      pokeByteOff ptr n (LBS.index bs (fromIntegral n))
      go (n - 1)

writeHeader :: Ptr Word8 -> Int -> IO ()
writeHeader ptr len = do
  let header = encode (fromIntegral len :: Word32)
  writeLBSToPtr header ptr

readHeader :: Ptr Word8 -> IO Int
readHeader ptr = do
  b0 <- peekByteOff ptr 0
  b1 <- peekByteOff ptr 1
  b2 <- peekByteOff ptr 2
  b3 <- peekByteOff ptr 3
  return (fromIntegral (decode (LBS.pack [b0, b1, b2, b3]) :: Word32))

waitForHeader :: Ptr Word8 -> Int -> IO Int
waitForHeader ptr offset = go
  where
    go = do
      len <- readHeader (ptr `plusPtr` offset)
      -- TODO: This will break if we write a bytestring of length zero.
      if len == 0
      then go
      else return len

-- | "active" file becomes "dirty", and the "clean" file becomes the new
-- "active" file.
rotateFiles :: Journal -> IO ()
rotateFiles = undefined

-- Assumption: cleaning the dirty file takes shorter amount of time than filling
-- up the active file to its max size.
cleanDirtyFile :: Journal -> IO ()
cleanDirtyFile = undefined

spawnCleaningThread :: IO (Async ())
spawnCleaningThread = undefined

data Inconsistency
  = PartialReceived
  | PartialRotation

checkForInconsistencies :: Journal -> IO [Inconsistency]
checkForInconsistencies = undefined

fixInconsistency :: Inconsistency -> Journal -> IO ()
fixInconsistency = undefined
