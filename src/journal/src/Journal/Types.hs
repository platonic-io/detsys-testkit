module Journal.Types
  ( Journal(Journal)
  , jMaxByteSize
  , jOffset
  , jDirectory
  , jBytesConsumed
  , Options(Options)
  , oMaxByteSize
  , JournalConsumer(JournalConsumer)
  , jcBytesConsumed
  , jcDirectory
  , jcMaxByteSize
  , newJournalPtrRef
  , readJournalPtr
  , updateJournalPtr
  , newJournalConsumerPtrRef
  , readJournalConsumerPtr
  , updateJournalConsumerPtr
  , getMaxByteSize
  , readFileCount
  , bumpFileCount
  , module Journal.Types.AtomicCounter)
  where

import Control.Concurrent.STM
import Control.Concurrent.STM (TVar)
import Data.ByteString (ByteString)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.Word (Word32, Word64, Word8)
import Foreign.Ptr (Ptr, plusPtr)

import Journal.Types.AtomicCounter

------------------------------------------------------------------------

data Journal = Journal
  { jPtr           :: {-# UNPACK #-} !(TVar (Ptr Word8))
  , jOffset        :: {-# UNPACK #-} !AtomicCounter
  , jMaxByteSize   :: {-# UNPACK #-} !Int
  , jDirectory     ::                !FilePath
  , jBytesConsumed :: {-# UNPACK #-} !AtomicCounter -- jGatingBytes?
  , jFileCount     :: {-# UNPACK #-} !AtomicCounter
  -- , jMetrics :: Metrics
  }

newJournalPtrRef :: Ptr Word8 -> IO (TVar (Ptr Word8))
newJournalPtrRef = newTVarIO

readJournalPtr :: Journal -> IO (Ptr Word8)
readJournalPtr = atomically . readTVar . jPtr

updateJournalPtr :: Journal -> Ptr Word8 -> IO ()
updateJournalPtr jour ptr = atomically (writeTVar (jPtr jour) ptr)

getMaxByteSize :: Journal -> Int
getMaxByteSize = jMaxByteSize

readFileCount :: Journal -> IO Int
readFileCount = readCounter . jFileCount

bumpFileCount :: Journal -> IO ()
bumpFileCount = incrCounter_ 1 . jFileCount

data Metrics = Metrics
  { mAbortedConnections :: Word32
  , mReplaySize :: Int -- XXX: Histogram
  }

emptyMetrics :: Metrics
emptyMetrics = Metrics 0 0

data Options = Options
  { oMaxByteSize :: !Int }
  -- archive
  -- buffer and fsync every ms?
  -- max disk space in total? multiple of maxSize?
  -- checksum? none, crc32 or sha256?
  -- wait strategy?

data JournalConsumer = JournalConsumer
  { jcPtr           :: {-# UNPACK #-} !(IORef (Ptr Word8))
  , jcBytesConsumed :: {-# UNPACK #-} !AtomicCounter
  , jcDirectory     ::                !FilePath
  , jcMaxByteSize   :: {-# UNPACK #-} !Int
  }

newJournalConsumerPtrRef :: Ptr Word8 -> IO (IORef (Ptr Word8))
newJournalConsumerPtrRef = newIORef

readJournalConsumerPtr :: JournalConsumer -> IO (Ptr Word8)
readJournalConsumerPtr = readIORef . jcPtr

updateJournalConsumerPtr :: JournalConsumer -> Ptr Word8 -> IO ()
updateJournalConsumerPtr jc ptr = writeIORef (jcPtr jc) ptr
