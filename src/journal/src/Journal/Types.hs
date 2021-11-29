module Journal.Types
  ( Journal(Journal)
  , jMaxByteSize
  , jOffset
  , Options(Options)
  , JournalConsumer(JournalConsumer)
  , jcBytesConsumed
  , getJournalPtr
  , getJournalConsumerPtr
  , newJournalPtrRef
  , newJournalConsumerPtrRef
  , module Journal.Types.AtomicCounter)
  where

import Control.Concurrent.STM
import Control.Concurrent.STM (TVar)
import Data.ByteString (ByteString)
import Data.IORef (IORef, newIORef, readIORef)
import Data.Word (Word32, Word64, Word8)
import Foreign.Ptr (Ptr, plusPtr)

import Journal.Types.AtomicCounter

------------------------------------------------------------------------

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
