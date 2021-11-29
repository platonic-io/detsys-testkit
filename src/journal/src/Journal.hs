module Journal
  ( module Journal.Types
  , defaultOptions
  , startJournal
  , appendBS
  , tee
  , appendRecv
  , readJournal
  , truncateAfterSnapshot
  , replay
  , replay_
  ) where

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
    activeExists <- doesFileExist (dir </> "active")
    if activeExists
    then do
      nuls <- BS.length . BS.takeWhileEnd (== (fromIntegral 0)) <$>
                BS.readFile (dir </> "active")
      return (maxByteSize - nuls)
    else return 0

  (ptr, _rawSize, _offset, _size) <-
    mmapFilePtr (dir </> "active") ReadWriteEx (Just (fromIntegral offset, maxByteSize))
  -- XXX: assert max size
  bytesProducedCounter <- newCounter offset
  ptrRef <- newJournalPtrRef (ptr `plusPtr` offset)
  jcPtrRef <- newJournalConsumerPtrRef ptr
  fileCounter <- newCounter 0
  bytesConsumedCounter <- newCounter 0
  return (Journal ptrRef bytesProducedCounter maxByteSize fileCounter,
          JournalConsumer jcPtrRef bytesConsumedCounter)

------------------------------------------------------------------------

-- * Production

-- NOTE: pre-condition: `BS.length bs > 0`
appendBS :: Journal -> ByteString -> IO ()
appendBS jour bs = undefined

-- NOTE: pre-condition: `len` > 0
tee :: Journal -> Socket -> Int -> IO ByteString
tee jour sock len = do
  offset <- claim jour len
  buf <- getJournalPtr jour
  receivedBytes <- recvBuf sock (buf `plusPtr` (offset + hEADER_SIZE)) len
  writeHeader (buf `plusPtr` offset) len
  fptr <- newForeignPtr_ buf
  return (BS.copy (fromForeignPtr fptr (offset + hEADER_SIZE) len))

-- NOTE: pre-condition: `len` > 0
appendRecv :: Journal -> Socket -> Int -> IO Int
appendRecv jour sock len = do
  offset <- claim jour len
  buf <- getJournalPtr jour
  receivedBytes <- recvBuf sock (buf `plusPtr` (offset + hEADER_SIZE)) len
  writeHeader (buf `plusPtr` offset) len
  return receivedBytes

------------------------------------------------------------------------

-- * Consumption

readJournal :: JournalConsumer -> IO ByteString
readJournal jc = do
  ptr <- getJournalConsumerPtr jc
  offset <- incrCounter hEADER_SIZE (jcBytesConsumed jc)
  len <- waitForHeader ptr offset
  fptr <- newForeignPtr_ ptr
  let bs = BS.copy (fromForeignPtr fptr (offset + hEADER_SIZE) len)
  incrCounter_ len (jcBytesConsumed jc)
  return bs

------------------------------------------------------------------------

-- * Snapshots and replay

truncateAfterSnapshot :: Journal -> Int -> IO ()
truncateAfterSnapshot jour bytesRead = undefined

replay :: Journal -> (ByteString -> IO a) -> IO [a]
replay = undefined

replay_ :: Journal -> (ByteString -> IO ()) -> IO ()
replay_ = undefined
