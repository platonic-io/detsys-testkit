module Journal.Types where

import Control.Concurrent.STM (TVar)
import Data.ByteString (ByteString)
import Control.Concurrent.STM
import Data.IORef (IORef, newIORef, readIORef, atomicModifyIORef')
import Data.Word (Word8, Word32, Word64)
import Foreign.Ptr (Ptr, plusPtr)

------------------------------------------------------------------------

newtype AtomicCounter = AtomicCounter (IORef Int)

newCounter :: Int -> IO AtomicCounter
newCounter i = AtomicCounter <$> newIORef i

incrCounter :: Int -> AtomicCounter -> IO Int
incrCounter i (AtomicCounter ref) = atomicModifyIORef' ref (\j -> (i + j, j))

incrCounter_ :: Int -> AtomicCounter -> IO ()
incrCounter_ i (AtomicCounter ref) = atomicModifyIORef' ref (\j -> (i + j, ()))

readCounter :: AtomicCounter -> IO Int
readCounter (AtomicCounter ref) = readIORef ref

data Journal = Journal
  { jPtr          :: !(TVar (Ptr Word8))
  , jOffset       :: {-# UNPACK #-} !AtomicCounter
  , jMaxByteSize  :: {-# UNPACK #-} !Int
  , jFileCount    :: {-# UNPACK #-} !AtomicCounter
  -- , jFile     :: {-# UNPACK #-} !(TVar FilePath)
  -- , jOffset   :: {-# UNPACK #-} !(TVar Word64) -- Tail.
  -- , jSequence :: {-# UNPACK #-} !(TVar Word64)
  -- -- jPointerToActiveFile
  -- -- jGatingBytes :: IORef Word64
  -- , jMetrics :: Metrics
  }

newJournalPtrRef :: Ptr Word8 -> IO (TVar (Ptr Word8))
newJournalPtrRef = newTVarIO

getJournalPtr :: Journal -> IO (Ptr Word8)
getJournalPtr = atomically . readTVar . jPtr

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

data JournalConsumer = JournalConsumer
   { jcPtr           :: {-# UNPACK #-} !(IORef (Ptr Word8))
   , jcBytesConsumed :: {-# UNPACK #-} !AtomicCounter
   }

newJournalConsumerPtrRef :: Ptr Word8 -> IO (IORef (Ptr Word8))
newJournalConsumerPtrRef = newIORef

getJournalConsumerPtr :: JournalConsumer -> IO (Ptr Word8)
getJournalConsumerPtr = readIORef . jcPtr
