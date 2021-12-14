{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Journal.Internal where

import Control.Concurrent (threadDelay)
import Control.Concurrent.STM (atomically, writeTVar)
import Control.Exception (assert)
import Control.Monad (unless, when)
import Data.Binary (Binary, decode, encode)
import Data.Bits (Bits, (.&.))
import qualified Data.ByteString as BS
import Data.ByteString.Internal (fromForeignPtr)
import qualified Data.ByteString.Lazy as LBS
import Data.List (isPrefixOf)
import Data.Maybe (catMaybes)
import Data.Word (Word32, Word8)
import Foreign.ForeignPtr (newForeignPtr_)
import Foreign.Ptr (Ptr, plusPtr)
import Foreign.Storable (Storable, peekByteOff, pokeByteOff, sizeOf)
import GHC.Stack (HasCallStack)
import System.Directory
       (copyFile, doesFileExist, listDirectory, renameFile)
import System.FilePath ((</>))
import System.IO.MMap (Mode(ReadWriteEx), mmapFilePtr, munmapFilePtr)

import Journal.Internal.Parse
import Journal.Types
import Journal.Types.AtomicCounter

------------------------------------------------------------------------

newtype HeaderTag = HeaderTag Word8
  deriving newtype (Eq, Binary, Bits, Show, Num, Storable)

newtype HeaderVersion = HeaderVersion Word8
  deriving newtype (Eq, Binary, Num, Storable)

newtype HeaderLength = HeaderLength Word32
  deriving newtype (Eq, Ord, Binary, Enum, Real, Integral, Num, Storable)

newtype HeaderIndex = HeaderIndex Word32
  deriving newtype (Eq, Binary, Num, Storable)

------------------------------------------------------------------------

-- * Constants

-- | The size of the journal entry header in bytes.
hEADER_SIZE :: Int
hEADER_SIZE
  = sizeOf (1 :: HeaderTag)
  + sizeOf (1 :: HeaderVersion)
  + sizeOf (4 :: HeaderLength)
  -- + sizeOf (4 :: HeaderIndex)
  -- XXX: CRC?

fOOTER_SIZE :: Int
fOOTER_SIZE = hEADER_SIZE

cURRENT_VERSION :: HeaderVersion
cURRENT_VERSION = 0

aCTIVE_FILE :: FilePath
aCTIVE_FILE = "active"

dIRTY_FILE :: FilePath
dIRTY_FILE = "dirty"

cLEAN_FILE :: FilePath
cLEAN_FILE = "clean"

sNAPSHOT_FILE :: FilePath
sNAPSHOT_FILE = "snapshot"

aRCHIVE_FILE :: FilePath
aRCHIVE_FILE = "archive"

------------------------------------------------------------------------

claim :: Journal -> Int -> IO Int
claim jour len = assert (hEADER_SIZE + len <= getMaxByteSize jour) $ do
  offset <- getAndIncrCounter (hEADER_SIZE + len) (jOffset jour)
  if offset + hEADER_SIZE + len + fOOTER_SIZE <= getMaxByteSize jour
  then do
    putStrLn ("claim, fits in current file, len: " ++ show len)
    return offset -- Fits in current file.
  else if offset <= getMaxByteSize jour
       then do
         -- First writer that overflowed the file, the second one would have got
         -- an `offset` grather than `getMaxByteSize jour`.

         putStrLn ("claim, first writer to overflow, offset: " ++ show offset)
         ptr <- readJournalPtr jour
         writePaddingFooter ptr offset (getMaxByteSize jour)
         rotateFiles jour
         writeCounter (jOffset jour) (hEADER_SIZE + len)
         return 0
       else do
         assertM (offset > getMaxByteSize jour)
         -- `offset > maxBytes`, so we clearly can't write to the current file.
         -- Wait for the first writer that overflowed to rotate the files then
         -- write.

         -- Check if header is written to offset (if that's the case the active
         -- file hasn't been rotated yet)
         undefined

mmapFile :: FilePath -> Int -> IO (Ptr Word8, Int)
mmapFile fp maxByteSize = do
  (ptr, rawSize, _offset, size) <-
    mmapFilePtr fp ReadWriteEx (Just (0, maxByteSize))
  assertM (size == maxByteSize)
  assertM (rawSize == maxByteSize)
  return (ptr, rawSize)

munmapFile :: Ptr Word8 -> Int -> IO ()
munmapFile = munmapFilePtr

-- | "active" file becomes "dirty", and the "clean" file becomes the new
-- "active" file.
rotateFiles :: Journal -> IO ()
rotateFiles jour = do
  renameFile (jDirectory jour </> aCTIVE_FILE) (jDirectory jour </> dIRTY_FILE)
  renameFile (jDirectory jour </> cLEAN_FILE)  (jDirectory jour </> aCTIVE_FILE)
  -- XXX: do we need to unmap the old active file ptr? Need raw size for that...
  (ptr, _rawSize) <- mmapFile (jDirectory jour </> aCTIVE_FILE) (jMaxByteSize jour)
  updateJournalPtr jour ptr
  cleanDirtyFile jour -- XXX: can be done async?

-- Assumption: if cleaning is done asynchronously then its assumed that cleaning
-- the dirty file takes shorter amount of time than filling up the active file
-- to its max size.
cleanDirtyFile :: Journal -> IO ()
cleanDirtyFile jour = do
  files <- listDirectory (jDirectory jour)
  let n = length (filter (aRCHIVE_FILE `isPrefixOf`) files)
  renameFile (jDirectory jour </> dIRTY_FILE) (jDirectory jour </> aRCHIVE_FILE ++ show n)
  (ptr, rawSize) <- mmapFile (jDirectory jour </> cLEAN_FILE) (jMaxByteSize jour)
  munmapFile ptr rawSize

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
  { jhTag      :: !HeaderTag
  , jhVersion  :: !HeaderVersion
  , jhLength   :: !HeaderLength
  -- , jhChecksum :: !Word32 -- V1
  }

pattern Empty   = 0 :: HeaderTag
pattern Valid   = 1 :: HeaderTag
pattern Invalid = 2 :: HeaderTag
pattern Padding = 4 :: HeaderTag

tagString :: HeaderTag -> String
tagString Empty   = "Empty"
tagString Valid   = "Valid"
tagString Invalid = "Invalid"
tagString Padding = "Padding"
tagString other   = "Unknown: " ++ show other

newHeader :: HeaderTag -> HeaderVersion -> HeaderLength -> JournalHeader
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

peekTag :: Ptr Word8 -> IO HeaderTag
peekTag ptr = HeaderTag <$> peekByteOff ptr 0

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

writePaddingFooter :: Ptr Word8 -> Int -> Int -> IO ()
writePaddingFooter ptr offset maxByteSize = assert (offset < maxByteSize) $ do
  let remLen = fromIntegral (maxByteSize - offset - hEADER_SIZE)
  writeHeader (ptr `plusPtr` offset) (newHeader Padding cURRENT_VERSION remLen)

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
        Padding -> return acc -- XXX: Or continue with the "next" file?

waitForHeader :: Ptr Word8 -> Int -> IO Int
waitForHeader ptr offset = go
  where
    go = do
      putStrLn ("waitForHeader: looking for header at offset: " ++ show offset)
      hdr <- readHeader (ptr `plusPtr` offset)
      if jhTag hdr == Empty
      then threadDelay 1000000 >> go -- XXX: wait strategy via options?
      else do
        -- XXX: Also assert that jhLength hdr <= maxByteSize)
        assertM ((jhTag hdr == Valid   && 0 <  jhLength hdr) ||
                 -- NOTE: Padding can be zero bytes.
                 (jhTag hdr == Padding && 0 <= jhLength hdr))
        return (fromIntegral (jhLength hdr))

mapHeadersUntil :: HeaderTag -> (JournalHeader -> JournalHeader) -> Ptr Word8 -> Int -> IO ()
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

------------------------------------------------------------------------

data Inconsistency
  = ActiveFileSizeMismatch Int Int
  | ActiveFileParseError String
  | PartialReceived
  | PartialRotation
  deriving Show

inconsistencyString :: Inconsistency -> String
inconsistencyString = show

inconsistenciesString :: [Inconsistency] -> String
inconsistenciesString = show . map inconsistencyString

checkForInconsistencies :: Journal -> IO [Inconsistency]
checkForInconsistencies jour = catMaybes <$> sequence
  [ do bs <- BS.readFile (jDirectory jour </> aCTIVE_FILE)
       if BS.length bs /= jMaxByteSize jour
       then return (Just (ActiveFileSizeMismatch (jMaxByteSize jour) (BS.length bs)))
       else return Nothing
  , do eFileAst <- parseFileAST (jDirectory jour </> aCTIVE_FILE)
       return (either (Just . ActiveFileParseError . show) (const Nothing) eFileAst)
  ]

fixInconsistency :: Inconsistency -> Journal -> IO ()
fixInconsistency = undefined

------------------------------------------------------------------------

assertM :: (HasCallStack, Monad m) => Bool -> m ()
assertM b = assert b (return ())

------------------------------------------------------------------------

-- * Debugging

dumpFile :: FilePath -> IO ()
dumpFile fp = do
  b <- doesFileExist fp
  if not b
  then putStrLn (fp ++ " doesn't exist")
  else do
    bs <- BS.readFile fp
    putStrLn "===="
    putStrLn (fp ++ " (" ++ show (BS.length bs) ++ " bytes)")
    go 0 0 bs
  where
    go ix totBytes bs
      | BS.null bs = do
          putStrLn ""
          putStrLn "===="
          putStrLn ""
          putStrLn ("Total bytes: " ++ show totBytes)
      | otherwise  = do

          let header :: BS.ByteString
              header = BS.take hEADER_SIZE bs

              bs' :: BS.ByteString
              bs' = BS.drop hEADER_SIZE bs

              tag :: HeaderTag
              tag = HeaderTag (BS.head header)

              version :: Word8
              version = BS.head (BS.tail header)

              len :: Word32
              len = decode (LBS.fromStrict (BS.take 4 (BS.drop 2 header)))

              body :: BS.ByteString
              body = BS.take (fromIntegral len) bs'

          putStrLn "----"

          if tag == Empty && BS.all (== fromIntegral 0) bs'
          then do
            putStrLn ("... (" ++ show (hEADER_SIZE + BS.length bs') ++ " bytes free)")
            putStrLn ""
            putStrLn "===="
            putStrLn ""
            putStrLn ("Total bytes: " ++ show (totBytes + hEADER_SIZE + BS.length bs'))
          else do

            putStrLn ("Index    " ++ show ix)
            putStrLn ("Tag:     " ++ tagString tag)
            putStrLn ("Version: " ++ show version)
            putStrLn ("Length:  " ++ show len)
            putStrLn ("Body:    " ++ show body)

            go (ix + 1)
               (totBytes + hEADER_SIZE + fromIntegral len)
               (BS.drop (fromIntegral len) bs')
