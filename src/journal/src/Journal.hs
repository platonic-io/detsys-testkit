module Journal
  ( module Journal.Types
  , defaultOptions
  , startJournal
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
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.ByteString.Internal (fromForeignPtr)
import Data.IORef (newIORef)
import Foreign.ForeignPtr (newForeignPtr_)
import Foreign.Ptr (plusPtr)
import Network.Socket (Socket, recvBuf)
import System.Directory
       (createDirectoryIfMissing, doesDirectoryExist, doesFileExist)
import System.FilePath ((</>))
import System.IO.MMap (Mode(ReadWriteEx), mmapFilePtr, munmapFilePtr)

import Journal.Internal
import Journal.Types

------------------------------------------------------------------------

-- * Initialisation

defaultOptions :: Options
defaultOptions = Options 1024

startJournal :: FilePath -> Options -> IO (Journal, JournalConsumer)
startJournal dir (Options maxByteSize) = do
  dirExists <- doesDirectoryExist dir
  unless dirExists (createDirectoryIfMissing True dir)

  offset <- do
    activeExists <- doesFileExist (dir </> activeFile)
    if activeExists
    then do
      nuls <- BS.length . BS.takeWhileEnd (== (fromIntegral 0)) <$>
                BS.readFile (dir </> activeFile)
      return (maxByteSize - nuls)
    else return 0

  (ptr, _rawSize, _offset, size) <-
    mmapFilePtr (dir </> activeFile) ReadWriteEx (Just (0, maxByteSize))
  assertM (size == maxByteSize)
  bytesProducedCounter <- newCounter offset
  ptrRef <- newJournalPtrRef ptr
  bytesConsumedCounter <- newCounter 0
  jc <- JournalConsumer <$> newJournalConsumerPtrRef ptr <*> pure bytesConsumedCounter
                        <*> pure dir
  return (Journal ptrRef bytesProducedCounter maxByteSize dir bytesConsumedCounter, jc)

------------------------------------------------------------------------

-- * Production

-- NOTE: pre-condition: `0 < BS.length bs <= maxByteSize`
appendBS :: Journal -> ByteString -> IO ()
appendBS jour bs = assert (0 < BS.length bs && BS.length bs <= jMaxByteSize jour) $ do
  let len = BS.length bs
  offset <- claim jour len
  buf <- getJournalPtr jour
  writeBSToPtr bs (buf `plusPtr` (offset + hEADER_SIZE))
  writeHeader (buf `plusPtr` offset) (makeValidHeader len)

-- NOTE: pre-condition: `0 < len <= maxByteSize`
tee :: Journal -> Socket -> Int -> IO ByteString
tee jour sock len = assert (0 < len && len <= jMaxByteSize jour) $ do
  offset <- claim jour len
  putStrLn ("tee: writing to offset: " ++ show offset)
  buf <- getJournalPtr jour
  receivedBytes <- recvBuf sock (buf `plusPtr` (offset + hEADER_SIZE)) len
  writeHeader (buf `plusPtr` offset) (makeValidHeader len)
  fptr <- newForeignPtr_ buf
  return (BS.copy (fromForeignPtr fptr (offset + hEADER_SIZE) len))

-- NOTE: pre-condition: `0 < len <= maxByteSize`
appendRecv :: Journal -> Socket -> Int -> IO Int
appendRecv jour sock len = assert (0 < len && len <= jMaxByteSize jour) $ do
  offset <- claim jour len
  buf <- getJournalPtr jour
  receivedBytes <- recvBuf sock (buf `plusPtr` (offset + hEADER_SIZE)) len
  -- XXX: if receivedBytes /= len or if sock isn't connected, or other failure
  -- modes of `recv(2)`?
  writeHeader (buf `plusPtr` offset) (makeValidHeader len)
  return receivedBytes

------------------------------------------------------------------------

-- * Consumption

readJournal :: JournalConsumer -> IO ByteString
readJournal jc = do
  ptr <- getJournalConsumerPtr jc
  offset <- getAndIncrCounter hEADER_SIZE (jcBytesConsumed jc)
  len <- waitForHeader ptr offset
  fptr <- newForeignPtr_ ptr
  let bs = BS.copy (fromForeignPtr fptr (offset + hEADER_SIZE) len)
  incrCounter_ len (jcBytesConsumed jc)
  return bs

------------------------------------------------------------------------

-- * Snapshots and replay

-- | NOTE: @saveSnapshot@ assumes the serialisation of the application @state@
-- was done at the point of @bytesConsumed@ having been processed by the
-- application.
saveSnapshot :: JournalConsumer -> ByteString -> Int -> IO ()
saveSnapshot jc state bytesConsumed = do
  -- b <- doesFileExist (jDirectory jour </> snapshotFile)
  -- XXX: snapshot header
  BS.writeFile (jcDirectory jc </> snapshotFile) state

truncateAfterSnapshot :: JournalConsumer -> Int -> IO ()
truncateAfterSnapshot jc bytesConsumed = do
  -- XXX: needs to get the "oldest" ptr...
  ptr <- getJournalConsumerPtr jc
  mapHeadersUntil Valid (\hdr -> hdr { jhTag = Invalid}) ptr bytesConsumed

loadSnapshot :: Journal -> IO (Maybe ByteString)
loadSnapshot jour = do
  -- XXX: load snapshot header
  b <- doesFileExist (jDirectory jour </> snapshotFile)
  if b
  then do
    bs <- BS.readFile (jDirectory jour </> snapshotFile)
    return (Just bs)
  else return Nothing

replay :: JournalConsumer -> (a -> ByteString -> a) -> a -> IO (Int, a)
replay jc f x = do
  ptr <- getJournalConsumerPtr jc
  iterJournal ptr (jcBytesConsumed jc) go (0, x)
  where
    go (n, acc) bs = (n + 1, f acc bs)

------------------------------------------------------------------------

-- * Debugging

dumpJournal :: Journal -> IO ()
dumpJournal jour = do
  active <- BS.readFile (jDirectory jour </> activeFile)
  print active
