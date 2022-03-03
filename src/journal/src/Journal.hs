module Journal
  ( module Journal.Types
  , defaultOptions
  , allocateJournal
  , startJournal
  , appendBS
  -- , tee
  -- , appendRecv
  , readJournal
  -- , saveSnapshot
  -- , truncateAfterSnapshot
  -- , loadSnapshot
  -- , replay
  , dumpJournal
  , metricsBytesWritten
  ) where

import Control.Exception (assert, bracket)
import Control.Monad (unless, when, forM_)
import Data.Bits (popCount)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSChar8
import Data.ByteString.Internal (fromForeignPtr)
import Data.IORef (newIORef)
import Data.Int (Int64)
import qualified Data.Vector as Vector
import Foreign.ForeignPtr (newForeignPtr_)
import Foreign.Ptr (plusPtr)
import Network.Socket (Socket, recvBuf)
import System.Directory
       ( createDirectoryIfMissing
       , doesDirectoryExist
       , doesFileExist
       , getFileSize
       , removeFile
       )
import System.FilePath (takeDirectory, (</>))
import System.Random (randomIO)

import Journal.Internal
import Journal.Internal.BufferClaim
import Journal.Internal.ByteBufferPtr
import Journal.Internal.FileAllocate (fileAllocate)
import Journal.Internal.Logger (Logger, ioLogger, logg)
import Journal.Internal.Mmap (sysconfPageSize)
import Journal.Internal.Utils
import Journal.Types
import Journal.Types.AtomicCounter

------------------------------------------------------------------------

-- * Initialisation and shutdown

defaultOptions :: Options
defaultOptions = Options (64 * 1024) ioLogger

allocateJournal :: FilePath -> Options -> IO ()
allocateJournal fp (Options termBufferLen logger) = do
  unless (popCount termBufferLen == 1) $
    -- XXX: check bounds
    error "allocateJournal: oTermBufferLength must be a power of 2"
  b <- doesFileExist fp
  size <- if b then getFileSize fp else return 0
  if size == 0
  then do
    logg logger ("allocateJournal, creating new journal: " ++ fp)
    let dir = takeDirectory fp
    dirExists <- doesDirectoryExist dir
    unless dirExists (createDirectoryIfMissing True dir)

    let logLength = termBufferLen * pARTITION_COUNT + lOG_META_DATA_LENGTH

    fallocate fp (fromIntegral logLength)
    bb <- mmapped fp logLength
    meta <- wrapPart bb (logLength - lOG_META_DATA_LENGTH) lOG_META_DATA_LENGTH

    writeTermLength meta (fromIntegral termBufferLen)
    initTermId <- TermId <$> randomIO
    writeInitialTermId meta initTermId
    forM_ [0 .. pARTITION_COUNT - 1] $ \i ->
      initialiseTailWithTermId (Metadata meta) (PartitionIndex i)
        (initTermId + TermId (int2Int32 (i - if i == 0 then 0 else pARTITION_COUNT)))
    pageSize <- sysconfPageSize
    writePageSize (Metadata meta) (int2Int32 pageSize)
  else do
    logg logger ("allocateJournal, journal exists: " ++ fp)
    let logLength = termBufferLen * pARTITION_COUNT + lOG_META_DATA_LENGTH
    unless (size == toInteger logLength) $
      error "allocateJournal: file size doesn't match with log length"
    bb <- mmapped fp logLength
    meta <- Metadata <$> wrapPart bb (logLength - lOG_META_DATA_LENGTH) lOG_META_DATA_LENGTH
    termBufferLen' <- readTermLength meta
    unless (int322Int termBufferLen' == termBufferLen) $
      error "allocateJournal: oTermBufferLength doesn't match the metadata"
    pageSize <- sysconfPageSize
    pageSize' <- readPageSize meta
    unless (int322Int pageSize' == pageSize) $
      error "allocateJournal: pageSize doesn't match the metadata"

startJournal :: FilePath -> Options -> IO Journal
startJournal fp (Options termLength logger) = do
  logLength <- fromIntegral <$> getFileSize fp
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
  return (Journal termBuffers (Metadata meta) logger)

------------------------------------------------------------------------

-- * Production

appendBS :: Journal -> ByteString -> IO (Either AppendError ())
appendBS jour bs = do
  assertIO $ do
    termBufferLen <- int322Int <$> readTermLength (jMetadata jour)
    return (0 < BS.length bs && align (hEADER_LENGTH + BS.length bs) fRAME_ALIGNMENT <=
            termBufferLen `div` 2)
  let len = BS.length bs
  eClaim <- tryClaim jour len
  case eClaim of
    Left err -> return (Left err)
    Right (_offset, bufferClaim) -> do
      putBS bufferClaim hEADER_LENGTH bs
      Right <$> commit bufferClaim (jLogger jour)

-- tee :: Journal -> Socket -> Int -> IO ByteString
-- tee jour sock len = do
--   assertM (0 < len && hEADER_LENGTH + len + fOOTER_LENGTH <= jMaxByteSize jour)
--   offset <- claim jour len
--   putStrLn ("tee: writing to offset: " ++ show offset)
--   buf <- readJournalPtr jour
--   receivedBytes <- recvBuf sock (buf `plusPtr` (offset + hEADER_LENGTH)) len
--   writeHeader (buf `plusPtr` offset) (makeValidHeader len)
--   fptr <- newForeignPtr_ buf
--   return (BS.copy (fromForeignPtr fptr (offset + hEADER_LENGTH) len))

recvBytes :: BufferClaim -> Socket -> Int -> IO Int
recvBytes bc sock len = withPtr bc $ \ptr -> recvBuf sock ptr len

-- appendRecv :: Journal -> Socket -> Int -> IO Int
-- appendRecv jour sock len = do
--   assertM (0 < len && hEADER_LENGTH + len + fOOTER_LENGTH <= jMaxByteSize jour)
--   offset <- claim jour len
--   buf <- readJournalPtr jour
--   receivedBytes <- recvBuf sock (buf `plusPtr` (offset + hEADER_LENGTH)) len
--   -- XXX: if receivedBytes /= len or if sock isn't connected, or other failure
--   -- modes of `recv(2)`?
--   writeHeader (buf `plusPtr` offset) (makeValidHeader len)
--   return receivedBytes

------------------------------------------------------------------------

-- * Consumption

readJournal :: Journal -> IO (Maybe ByteString)
readJournal jour = do
  offset <- readBytesConsumed (jMetadata jour)
  let jLog = logg (jLogger jour)
  jLog ("readJournal, offset: " ++ show offset)

  termLen <- readTermLength (jMetadata jour)
  let readIndex = indexByPosition (int2Int64 offset) (positionBitsToShift termLen)
  jLog ("readJournal, readIndex: " ++ show (unPartitionIndex readIndex))

  termCount <- activeTermCount (jMetadata jour)
  let activeTermIndex = indexByTermCount termCount
  rawTail <- readRawTail (jMetadata jour) activeTermIndex
  let termBuffer = jTermBuffers jour Vector.! unPartitionIndex readIndex
      activeTermId = rawTailTermId rawTail
      termOffset = rawTailTermOffset rawTail termLen

  jLog ("readJournal, termOffset: " ++ show (unTermOffset termOffset))
  initTermId <- readInitialTermId (jMetadata jour)
  jLog ("readJournal, initTermId: " ++ show (unTermId initTermId))
  let position =
        computePosition activeTermId termOffset (positionBitsToShift termLen) initTermId
  assertM (int2Int64 offset <= position)

  let readTermCount =
        computeTermIdFromPosition (int2Int64 offset) (positionBitsToShift termLen) initTermId
        - unTermId initTermId

  jLog ("readJournal, readTermCount: " ++ show readTermCount)

  if int2Int64 offset == position
  then return Nothing
  else do
    assertM (int2Int64 offset < position)

    let relativeOffset = int2Int32 (align offset fRAME_ALIGNMENT) - readTermCount * termLen
    jLog ("readJournal, relativeOffset: " ++ show relativeOffset)
    tag <- readFrameType termBuffer (TermOffset relativeOffset)
    jLog ("readJournal, tag: " ++ show tag)
    HeaderLength len <- readFrameLength termBuffer (TermOffset relativeOffset)
    jLog ("readJournal, len: " ++ show len)
    if tag == Padding
    then do
      assertM (len >= 0)
      incrBytesConsumed_ (jMetadata jour) (align (int322Int len) fRAME_ALIGNMENT)
      jLog "readJournal, skipping padding..."
      readJournal jour
    else do
      assertM (len > 0)
      jLog ("readJournal, termCount: " ++ show (unTermCount termCount))
      bs <- getByteStringAt termBuffer
              (int322Int relativeOffset + hEADER_LENGTH)
              (int322Int len - hEADER_LENGTH)
      assertM (BS.length bs == int322Int len - hEADER_LENGTH)
      incrBytesConsumed_ (jMetadata jour) (align (int322Int len) fRAME_ALIGNMENT)
      return (Just bs)

------------------------------------------------------------------------

-- * Snapshots and replay

-- | NOTE: @saveSnapshot@ assumes the serialisation of the application @state@
-- was done at the point of @bytesConsumed@ having been processed by the
-- application.
-- saveSnapshot :: JournalConsumer -> ByteString -> Int -> IO ()
-- saveSnapshot jc state bytesConsumed = do
--   -- b <- doesFileExist (jDirectory jour </> snapshotFile)
--   -- XXX: snapshot header
--   BS.writeFile (jcDirectory jc </> sNAPSHOT_FILE) state
--
-- truncateAfterSnapshot :: JournalConsumer -> Int -> IO ()
-- truncateAfterSnapshot jc bytesConsumed = do
--   -- XXX: needs to get the "oldest" ptr...
--   ptr <- readJournalConsumerPtr jc
--   mapHeadersUntil Valid (\hdr -> hdr { jhTag = Invalid }) ptr bytesConsumed
--
-- loadSnapshot :: Journal -> IO (Maybe ByteString)
-- loadSnapshot jour = do
--   -- XXX: load snapshot header
--   b <- doesFileExist (jDirectory jour </> sNAPSHOT_FILE)
--   if b
--   then do
--     bs <- BS.readFile (jDirectory jour </> sNAPSHOT_FILE)
--     return (Just bs)
--   else return Nothing
--
-- replay :: JournalConsumer -> (a -> ByteString -> a) -> a -> IO (Int, a)
-- replay jc f x = do
--   ptr <- readJournalConsumerPtr jc
--   iterJournal ptr (jcBytesConsumed jc) go (0, x)
--   where
--     go (n, acc) bs = (n + 1, f acc bs)

------------------------------------------------------------------------

-- * Debugging

dumpJournal :: Journal -> IO ()
dumpJournal jour = do
  termLen <- readTermLength (jMetadata jour)
  termOffsets <- Vector.generateM pARTITION_COUNT $ \i -> do
    rawTail <- readRawTail (jMetadata jour) (PartitionIndex i)
    return (rawTailTermOffset rawTail termLen)
  Vector.imapM_ dumpTermBuffer (jTermBuffers jour `Vector.zip` termOffsets)
  dumpMetadata (jMetadata jour)

------------------------------------------------------------------------

-- * Metrics

metricsBytesWritten :: Journal -> IO Int64
metricsBytesWritten jour = do
  let meta = jMetadata jour
  termCount <- activeTermCount meta
  initTermId <- readInitialTermId meta
  termLen <- readTermLength meta
  let index                = indexByTermCount termCount
  rawTail <- readRawTail meta index
  let termId     = rawTailTermId rawTail
      termOffset = rawTailTermOffset rawTail termLen
  return (computePosition termId termOffset (positionBitsToShift termLen) initTermId)
