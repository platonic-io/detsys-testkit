{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Journal.Internal where

import Control.Concurrent (threadDelay)
import Control.Exception (assert)
import Control.Monad (when)
import Data.Binary (decode, encode)
import Data.Bits ((.&.))
import qualified Data.ByteString as BS
import Data.ByteString.Internal (fromForeignPtr)
import qualified Data.ByteString.Lazy as LBS
import Data.Word (Word32, Word8)
import Foreign.ForeignPtr (newForeignPtr_)
import Foreign.Ptr (Ptr, plusPtr)
import Foreign.Storable (peekByteOff, pokeByteOff)

import Journal.Types
import Journal.Types.AtomicCounter

------------------------------------------------------------------------

-- | The size of the journal entry header in bytes.
hEADER_SIZE :: Int
hEADER_SIZE = 1 + 1 + 4 -- sizeOf Word8 + sizeOf Word8 + sizeOf Word32
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

type JournalHeader = JournalHeaderV0

data JournalHeaderV0 = JournalHeaderV0
  { jhTag      :: !Word8
  , jhVersion  :: !Word8
  , jhLength   :: !Word32
  -- , jhChecksum :: !Word32 -- V1
  }

pattern Empty   = 0 :: Word8
pattern Valid   = 1 :: Word8
pattern Invalid = 2 :: Word8

cURRENT_VERSION :: Word8
cURRENT_VERSION = 0

newHeader :: Word8 -> Word8 -> Word32 -> JournalHeader
newHeader = JournalHeaderV0

makeValidHeader :: Int -> JournalHeader
makeValidHeader len = newHeader Valid cURRENT_VERSION (fromIntegral len)

writeHeader :: Ptr Word8 -> JournalHeader -> IO ()
writeHeader ptr hdr =
  assert (LBS.length header == fromIntegral hEADER_SIZE) $
    writeLBSToPtr header ptr
  where
    header :: LBS.ByteString
    header = mconcat [ encode (jhTag hdr)
                     , encode (jhVersion hdr)
                     , encode (jhLength hdr)
                     ]

readHeader :: Ptr Word8 -> IO JournalHeader
readHeader ptr = do
  b0 <- peekByteOff ptr 0 -- tag     (1 byte)
  b1 <- peekByteOff ptr 1 -- version (1 byte)
  b2 <- peekByteOff ptr 2 -- length  (4 bytes)
  b3 <- peekByteOff ptr 3
  b4 <- peekByteOff ptr 4
  b5 <- peekByteOff ptr 5
  -- XXX: decodeOrFail?
  -- NOTE: Data.Binary always uses network order (big-endian).
  return (newHeader
           (decode (LBS.pack [b0]))
           (decode (LBS.pack [b1]))
           (decode (LBS.pack [b2, b3, b4, b5])))

headerExists :: Ptr Word8 -> Int -> IO Bool
headerExists ptr offset = do
  hdr <- readHeader (ptr `plusPtr` offset)
  return (jhTag hdr /= Empty)

nextHeader :: JournalConsumer -> Ptr Word8 -> IO (Maybe JournalHeader)
nextHeader jc ptr = do
  hdr <- readHeader ptr
  case jhTag hdr of
    Valid   -> return (Just hdr)
    Invalid -> nextHeader jc (ptr `plusPtr` (hEADER_SIZE + fromIntegral (jhLength hdr)))
    Empty   -> return Nothing

iterJournal :: Ptr Word8 -> AtomicCounter -> (a -> BS.ByteString -> a) -> a -> IO a
iterJournal ptr consumed f x = do
  offset <- readCounter consumed
  go offset x
  where
    go offset acc = do
      hdr <- readHeader (ptr `plusPtr` offset)
      case jhTag hdr of
        Empty   -> return acc
        Valid   -> do
          fptr <- newForeignPtr_ ptr
          let len = fromIntegral (jhLength hdr)
          incrCounter_ (hEADER_SIZE + len) consumed
          go (offset + hEADER_SIZE + len)
             (f acc (BS.copy (fromForeignPtr fptr (offset + hEADER_SIZE) len)))
        Invalid -> do
          incrCounter_ (hEADER_SIZE + fromIntegral (jhLength hdr)) consumed
          go (offset + hEADER_SIZE + fromIntegral (jhLength hdr)) acc

waitForHeader :: Ptr Word8 -> Int -> IO Int
waitForHeader ptr offset = go
  where
    go = do
      hdr <- readHeader (ptr `plusPtr` offset)
      if jhTag hdr == Empty
      then threadDelay 1000 >> go -- XXX: wait strategy via options?
      else return (fromIntegral (jhLength hdr))

mapHeadersUntil :: Word8 -> (JournalHeader -> JournalHeader) -> Ptr Word8 -> Int -> IO ()
mapHeadersUntil mask f ptr limit = go 0
  where
    go :: Int -> IO ()
    go offset
      | offset == limit = return ()
      | otherwise = do
          assertM (offset < limit)
          hdr <- readHeader (ptr `plusPtr` offset)
          -- Only apply @f@ if the tag is in @mask@ (`= tag0 .|. ... .|. tagN`).
          when (jhTag hdr .&. mask /= 0) $
            writeHeader (ptr `plusPtr` offset) (f hdr)
          go (offset + hEADER_SIZE + fromIntegral (jhLength hdr))

-- | "active" file becomes "dirty", and the "clean" file becomes the new
-- "active" file.
rotateFiles :: Journal -> IO ()
rotateFiles = undefined

-- Assumption: cleaning the dirty file takes shorter amount of time than filling
-- up the active file to its max size.
cleanDirtyFile :: Journal -> IO ()
cleanDirtyFile = undefined

------------------------------------------------------------------------

data Inconsistency
  = PartialReceived
  | PartialRotation

checkForInconsistencies :: Journal -> IO [Inconsistency]
checkForInconsistencies = undefined

fixInconsistency :: Inconsistency -> Journal -> IO ()
fixInconsistency = undefined


------------------------------------------------------------------------

assertM :: Monad m => Bool -> m ()
assertM b = assert b (return ())
