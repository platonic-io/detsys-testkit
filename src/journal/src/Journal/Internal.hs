module Journal.Internal where

import Control.Concurrent (threadDelay)
import Data.Binary (decode, encode)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Data.Word (Word32, Word8)
import Foreign.Ptr (Ptr, plusPtr)
import Foreign.Storable (peekByteOff, pokeByteOff)

import Journal.Types
import Journal.Types.AtomicCounter

------------------------------------------------------------------------

-- | The size of the journal entry header in bytes.
hEADER_SIZE :: Int
hEADER_SIZE = 4 -- sizeOf (0 :: Word32)
  -- XXX: Some special header start byte? e.g. 'h' (header) and 'i' (ignore/trucated)
  -- XXX: version?
  -- XXX: CRC?

activeFile :: FilePath
activeFile = "active"

snapshotFile :: FilePath
snapshotFile = "snapshot"

claim :: Journal -> Int -> IO Int
claim jour len = do
  offset <- getAndIncrCounter (len + hEADER_SIZE) (jOffset jour)
  -- XXX: mod/.&. maxByteSize?
  if offset + len <= jMaxByteSize jour
  then return offset -- Fits in current file.
  else if offset < jMaxByteSize jour
       then do
         -- First writer that overflowed the file, the second one
         -- would have got an offset higher than `maxBytes`.

         -- rotate
         undefined
       else do
         -- `offset >= maxBytes`, so we clearly can't write to the current file.
         -- Wait for the first writer that overflowed to rotate the files then
         -- write.

         -- Check if header is written to offset (if that's the case the active
         -- file hasn't been rotated yet)
         undefined

writeBSToPtr :: BS.ByteString -> Ptr Word8 -> IO ()
writeBSToPtr bs ptr | BS.null bs = return ()
                    | otherwise  = go (fromIntegral (BS.length bs - 1))
  where
    go :: Int -> IO ()
    go 0 = pokeByteOff ptr 0 (BS.index bs 0)
    go n = do
      pokeByteOff ptr n (BS.index bs (fromIntegral n))
      go (n - 1)

-- XXX: Use Data.Primitive.ByteArray.copyMutableByteArrayToPtr instead?
writeLBSToPtr :: LBS.ByteString -> Ptr Word8 -> IO ()
writeLBSToPtr bs ptr | LBS.null bs = return ()
                     | otherwise   = go (fromIntegral (LBS.length bs - 1))
  where
    go :: Int -> IO ()
    go 0 = pokeByteOff ptr 0 (LBS.index bs 0)
    go n = do
      pokeByteOff ptr n (LBS.index bs (fromIntegral n))
      go (n - 1)

writeHeader :: Ptr Word8 -> Int -> IO ()
writeHeader ptr len = writeLBSToPtr header ptr
  where
    header :: LBS.ByteString
    header = encode (fromIntegral len :: Word32)

readHeader :: Ptr Word8 -> IO Int
readHeader ptr = do
  b0 <- peekByteOff ptr 0
  b1 <- peekByteOff ptr 1
  b2 <- peekByteOff ptr 2
  b3 <- peekByteOff ptr 3
  -- XXX: decodeOrFail?
  return (fromIntegral (decode (LBS.pack [b0, b1, b2, b3]) :: Word32))

headerExists :: Ptr Word8 -> Int -> IO Bool
headerExists ptr offset = do
  len <- readHeader (ptr `plusPtr` offset)
  -- TODO: This will break if we write a bytestring of length zero.
  return (len /= 0)

waitForHeader :: Ptr Word8 -> Int -> IO Int
waitForHeader ptr offset = go
  where
    go = do
      len <- readHeader (ptr `plusPtr` offset)
      -- TODO: This will break if we write a bytestring of length zero.
      if len == 0
      then threadDelay 1000 >> go -- XXX: wait strategy via options?
      else return len

-- | "active" file becomes "dirty", and the "clean" file becomes the new
-- "active" file.
rotateFiles :: Journal -> IO ()
rotateFiles = undefined

-- Assumption: cleaning the dirty file takes shorter amount of time than filling
-- up the active file to its max size.
cleanDirtyFile :: Journal -> IO ()
cleanDirtyFile = undefined

data Inconsistency
  = PartialReceived
  | PartialRotation

checkForInconsistencies :: Journal -> IO [Inconsistency]
checkForInconsistencies = undefined

fixInconsistency :: Inconsistency -> Journal -> IO ()
fixInconsistency = undefined
