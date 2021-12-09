module Journal
  ( module Journal.Types
  , defaultOptions
  , startJournal
  , stopJournal
  , appendBS
  , tee
  , appendRecv
  , readJournal
  , saveSnapshot
  , truncateAfterSnapshot
  , loadSnapshot
  , replay
  , dumpJournal
  ) where

import Control.Exception (assert)
import Control.Monad (unless)
import Data.Bits (popCount)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.ByteString.Internal (fromForeignPtr)
import Data.IORef (newIORef)
import Data.Word (Word32, Word8)
import Foreign.ForeignPtr (newForeignPtr_)
import Foreign.Ptr (plusPtr)
import Network.Socket (Socket, recvBuf)
import System.Directory
       (createDirectoryIfMissing, doesDirectoryExist, doesFileExist)
import System.FilePath ((</>))

import Journal.Internal
import Journal.Types

------------------------------------------------------------------------

-- * Initialisation and shutdown

defaultOptions :: Options
defaultOptions = Options 1024

startJournal :: FilePath -> Options -> IO (Journal, JournalConsumer)
startJournal dir (Options maxByteSize) = do
  unless (popCount maxByteSize == 1) $
    -- NOTE: The use of bitwise and (`.&.`) in `claim` relies on this.
    error "startJournal: oMaxByteSize must be a power of 2"
  dirExists <- doesDirectoryExist dir
  unless dirExists (createDirectoryIfMissing True dir)

  offset <- do
    activeExists <- doesFileExist (dir </> aCTIVE_FILE)
    if activeExists
    then do
      nuls <- BS.length . BS.takeWhileEnd (== (fromIntegral 0)) <$>
                BS.readFile (dir </> aCTIVE_FILE)
      return (maxByteSize - nuls)
    else return 0

  (ptr, _rawSize) <- mmapFile (dir </> aCTIVE_FILE) maxByteSize

  cleanExists <- doesFileExist (dir </> cLEAN_FILE)
  if cleanExists
  then do
    -- XXX: check that it actually is clean
    return ()
  else do
    (ptr, rawSize) <- mmapFile (dir </> cLEAN_FILE) maxByteSize
    munmapFile ptr rawSize

  bytesProducedCounter <- newCounter offset
  ptrRef <- newJournalPtrRef ptr
  bytesConsumedCounter <- newCounter 0
  jc <- JournalConsumer <$> newJournalConsumerPtrRef ptr <*> pure bytesConsumedCounter
                        <*> pure dir <*> pure maxByteSize
  fileCount <- newCounter 0 -- XXX: always start over from 0?
  return (Journal ptrRef bytesProducedCounter maxByteSize dir bytesConsumedCounter fileCount,
          jc)

stopJournal :: Journal -> JournalConsumer -> IO ()
stopJournal jour jc = do
  jPtr <- readJournalPtr jour
  munmapFile jPtr (jMaxByteSize jour)
  jcPtr <- readJournalConsumerPtr jc
  munmapFile jcPtr (jMaxByteSize jour)

------------------------------------------------------------------------

-- * Production

-- NOTE: pre-condition: `0 < BS.length bs <= maxByteSize`
appendBS :: Journal -> ByteString -> IO ()
appendBS jour bs = assert (0 < BS.length bs &&
                           hEADER_SIZE + BS.length bs <= jMaxByteSize jour) $ do
  let len = BS.length bs
  offset <- claim jour len
  buf <- readJournalPtr jour
  writeBSToPtr bs (buf `plusPtr` (offset + hEADER_SIZE))
  writeHeader (buf `plusPtr` offset) (makeValidHeader len)

-- NOTE: pre-condition: `0 < len <= maxByteSize`
tee :: Journal -> Socket -> Int -> IO ByteString
tee jour sock len = assert (0 < len && len <= jMaxByteSize jour) $ do
  offset <- claim jour len
  putStrLn ("tee: writing to offset: " ++ show offset)
  buf <- readJournalPtr jour
  receivedBytes <- recvBuf sock (buf `plusPtr` (offset + hEADER_SIZE)) len
  writeHeader (buf `plusPtr` offset) (makeValidHeader len)
  fptr <- newForeignPtr_ buf
  return (BS.copy (fromForeignPtr fptr (offset + hEADER_SIZE) len))

-- NOTE: pre-condition: `0 < len <= maxByteSize`
appendRecv :: Journal -> Socket -> Int -> IO Int
appendRecv jour sock len = assert (0 < len && len <= jMaxByteSize jour) $ do
  offset <- claim jour len
  buf <- readJournalPtr jour
  receivedBytes <- recvBuf sock (buf `plusPtr` (offset + hEADER_SIZE)) len
  -- XXX: if receivedBytes /= len or if sock isn't connected, or other failure
  -- modes of `recv(2)`?
  writeHeader (buf `plusPtr` offset) (makeValidHeader len)
  return receivedBytes

------------------------------------------------------------------------

-- * Consumption

readJournal :: JournalConsumer -> IO ByteString
readJournal jc = do
  ptr <- readJournalConsumerPtr jc
  offset <- readCounter (jcBytesConsumed jc)
  len <- waitForHeader ptr offset
  putStrLn ("readJournal, offset: " ++ show offset)
  putStrLn ("readJournal, len: " ++ show len)
  fptr <- newForeignPtr_ ptr
  let bs = BS.copy (fromForeignPtr fptr (offset + hEADER_SIZE) len)
  bytesRead <- incrCounter (hEADER_SIZE + len) (jcBytesConsumed jc)
  putStrLn ("readJournal, bytesRead: " ++ show bytesRead)
  if bytesRead == jcMaxByteSize jc
  then do
    putStrLn "readJournal, rotating..."
    tag <- peekTag (ptr `plusPtr` offset)
    if tag == Padding
    then do
      putStrLn "readJournal, skipping padding..."
      ptr <- readJournalConsumerPtr jc
      munmapFile ptr (jcMaxByteSize jc)
      (ptr', _rawSize) <- mmapFile (jcDirectory jc </> aCTIVE_FILE) (jcMaxByteSize jc)
      updateJournalConsumerPtr jc ptr'
      writeCounter (jcBytesConsumed jc) 0
      readJournal jc
    else do
      assertM (BS.head bs == Valid)
      return bs
  else return bs

------------------------------------------------------------------------

-- * Snapshots and replay

-- | NOTE: @saveSnapshot@ assumes the serialisation of the application @state@
-- was done at the point of @bytesConsumed@ having been processed by the
-- application.
saveSnapshot :: JournalConsumer -> ByteString -> Int -> IO ()
saveSnapshot jc state bytesConsumed = do
  -- b <- doesFileExist (jDirectory jour </> snapshotFile)
  -- XXX: snapshot header
  BS.writeFile (jcDirectory jc </> sNAPSHOT_FILE) state

truncateAfterSnapshot :: JournalConsumer -> Int -> IO ()
truncateAfterSnapshot jc bytesConsumed = do
  -- XXX: needs to get the "oldest" ptr...
  ptr <- readJournalConsumerPtr jc
  mapHeadersUntil Valid (\hdr -> hdr { jhTag = Invalid }) ptr bytesConsumed

loadSnapshot :: Journal -> IO (Maybe ByteString)
loadSnapshot jour = do
  -- XXX: load snapshot header
  b <- doesFileExist (jDirectory jour </> sNAPSHOT_FILE)
  if b
  then do
    bs <- BS.readFile (jDirectory jour </> sNAPSHOT_FILE)
    return (Just bs)
  else return Nothing

replay :: JournalConsumer -> (a -> ByteString -> a) -> a -> IO (Int, a)
replay jc f x = do
  ptr <- readJournalConsumerPtr jc
  iterJournal ptr (jcBytesConsumed jc) go (0, x)
  where
    go (n, acc) bs = (n + 1, f acc bs)

------------------------------------------------------------------------

-- * Debugging

dumpJournal :: Journal -> IO ()
dumpJournal jour = do
  dumpFile (jDirectory jour </> aCTIVE_FILE)
  dumpFile (jDirectory jour </> dIRTY_FILE)
  dumpFile (jDirectory jour </> cLEAN_FILE)
