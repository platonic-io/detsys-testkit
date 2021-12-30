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
import qualified Data.Vector as Vector
import Data.Word (Word32, Word8)
import Foreign.ForeignPtr (newForeignPtr_)
import Foreign.Ptr (plusPtr)
import Network.Socket (Socket, recvBuf)
import System.Directory
       (createDirectoryIfMissing, doesDirectoryExist, doesFileExist)
import System.FilePath (takeDirectory, (</>))

import Journal.Internal
import Journal.Internal.ByteBuffer
import Journal.Internal.BufferClaim
import Journal.Types
import Journal.Types.AtomicCounter

------------------------------------------------------------------------

-- * Initialisation and shutdown

defaultOptions :: Options
defaultOptions = Options 1024 (64 * 1024)

allocateJournal :: FilePath -> Options -> IO ()
allocateJournal = undefined
  -- System.Posix.Fcntl.fileAllocate

startJournal' :: FilePath -> Options -> IO Journal'
startJournal' fp (Options _ termLength) = do
  unless (popCount termLength == 1) $
    -- NOTE: The use of bitwise and (`.&.`) in `claim` relies on this.
    -- XXX: check bounds
    error "startJournal: oTermBufferLength must be a power of 2"

  let dir = takeDirectory fp
  dirExists <- doesDirectoryExist dir
  unless dirExists (createDirectoryIfMissing True dir)

  let logLength = termLength * pARTITION_COUNT + lOG_META_DATA_LENGTH
  bb <- mmapped fp logLength
  meta <- wrapPart bb (logLength - lOG_META_DATA_LENGTH) lOG_META_DATA_LENGTH

  termBuffers <-
    Vector.generateM pARTITION_COUNT $ \i ->
      let
        offset = i * termLength
      in do
        writePosition bb (Position offset)
        writeLimit bb (Limit (offset + termLength))
        slice bb
  -- XXX: This counter needs to be persisted somehow (mmapped?) in order to be
  -- able to recover from restarts.
  bytesConsumedCounter <- newCounter 0
  return (Journal' termBuffers (Metadata meta) bytesConsumedCounter)

startJournal :: FilePath -> Options -> IO (Journal, JournalConsumer)
startJournal dir (Options maxByteSize _) = do
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

appendBS :: Journal -> ByteString -> IO ()
appendBS jour bs = do
  assertM (0 < BS.length bs &&
           hEADER_LENGTH + BS.length bs + fOOTER_LENGTH <= jMaxByteSize jour)
  let len = BS.length bs
  offset <- claim jour len
  buf <- readJournalPtr jour
  writeBSToPtr bs (buf `plusPtr` (offset + hEADER_LENGTH))
  writeHeader (buf `plusPtr` offset) (makeValidHeader len)

tee :: Journal -> Socket -> Int -> IO ByteString
tee jour sock len = do
  assertM (0 < len && hEADER_LENGTH + len + fOOTER_LENGTH <= jMaxByteSize jour)
  offset <- claim jour len
  putStrLn ("tee: writing to offset: " ++ show offset)
  buf <- readJournalPtr jour
  receivedBytes <- recvBuf sock (buf `plusPtr` (offset + hEADER_LENGTH)) len
  writeHeader (buf `plusPtr` offset) (makeValidHeader len)
  fptr <- newForeignPtr_ buf
  return (BS.copy (fromForeignPtr fptr (offset + hEADER_LENGTH) len))

recvBytes :: BufferClaim -> Socket -> Int -> IO Int
recvBytes bc sock len = withPtr bc $ \ptr -> recvBuf sock ptr len

appendRecv :: Journal -> Socket -> Int -> IO Int
appendRecv jour sock len = do
  assertM (0 < len && hEADER_LENGTH + len + fOOTER_LENGTH <= jMaxByteSize jour)
  offset <- claim jour len
  buf <- readJournalPtr jour
  receivedBytes <- recvBuf sock (buf `plusPtr` (offset + hEADER_LENGTH)) len
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
  let bs = BS.copy (fromForeignPtr fptr (offset + hEADER_LENGTH) len)
  bytesRead <- incrCounter (hEADER_LENGTH + len) (jcBytesConsumed jc)
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
      putStrLn ("readJournal, tag: " ++ tagString tag)
      assertM (HeaderTag (BS.head bs) == Valid)
      return bs
  else do
    putStrLn ("readJournal, returning: " ++ show bs ++
              " (" ++ show (BS.length bs) ++ " bytes)")
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
