module Journal.Types where

import Control.Concurrent.STM (TVar)
import Data.ByteString (ByteString)
import Control.Concurrent.STM
import Data.IORef (IORef, newIORef, readIORef, atomicModifyIORef')
import Data.Word (Word8, Word32, Word64)
import Foreign.Ptr (Ptr, plusPtr)

------------------------------------------------------------------------

newtype Bytes = Bytes Int

newtype BytesRead = BytesRead Int
newtype Offset = Offset Int

newtype BytesCounter = BytesCounter (IORef Int)

newBytesCounter :: Int -> IO BytesCounter
newBytesCounter offset = do
  ref <- newIORef offset
  return (BytesCounter ref)

data Journal = Journal
  { jPtr          :: !(TVar (Ptr Word8))
  , jBytesWritten :: !BytesCounter
  , jMaxByteSize  :: !Int
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

-- XXX: remove, can be (re)calculate dfrom jBytesWritten
advanceJournalPtr :: Journal -> Int -> IO ()
advanceJournalPtr jour bytes = atomically $
  modifyTVar' (jPtr jour) (`plusPtr` bytes)

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
   { jcBytesRead :: IORef BytesRead
   }
