module Journal
  ( module Journal.Types
  , defaultOptions
  , startJournal
  , appendBS
  , tee
  , appendRecv
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
import System.FilePath ((</>))
import System.IO.MMap (Mode(ReadWriteEx), mmapFilePtr, munmapFilePtr)

import Journal.Types
import Journal.Internal

------------------------------------------------------------------------

-- * Initialisation

defaultOptions :: Options
defaultOptions = Options 1024

startJournal :: FilePath -> Options -> IO Journal
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

  putStrLn ("Offset: " ++ show offset)

  (ptr, _rawSize, _offset, _size) <-
    mmapFilePtr (dir </> "active") ReadWriteEx (Just (fromIntegral offset, maxByteSize))
  -- XXX: assert max size
  bytesWrittenCounter <- newCounter offset
  ptrRef <- newJournalPtrRef (ptr `plusPtr` offset)
  fileCounter <- newCounter 0
  return (Journal ptrRef bytesWrittenCounter maxByteSize fileCounter)

------------------------------------------------------------------------

-- * Production

appendBS :: Journal -> ByteString -> IO ()
appendBS = undefined

tee :: Journal -> Socket -> Int -> IO ByteString
tee jour sock len = do
  offset <- claim jour len
  putStrLn ("tee: offset = " ++ show offset)
  buf <- getJournalPtr jour
  receivedBytes <- recvBuf sock (buf `plusPtr` (offset + hEADER_SIZE)) len
  -- XXX: write header
  fptr <- newForeignPtr_ buf
  return (BS.copy (fromForeignPtr fptr offset len))

appendRecv :: Journal -> Socket -> Int -> IO Int
appendRecv jour sock len = do
  offset <- claim jour len
  buf <- getJournalPtr jour
  receivedBytes <- recvBuf sock (buf `plusPtr` (offset + hEADER_SIZE)) len
  -- XXX: write header
  return receivedBytes

------------------------------------------------------------------------

-- * Consumption

readJournal :: JournalConsumer -> IO ByteString
readJournal = undefined

------------------------------------------------------------------------

-- * Snapshots and replay

truncateAfterSnapshot :: Journal -> BytesRead -> IO ()
truncateAfterSnapshot = undefined

replay :: Journal -> (ByteString -> IO a) -> IO [a]
replay = undefined

replay_ :: Journal -> (ByteString -> IO ()) -> IO ()
replay_ = undefined
