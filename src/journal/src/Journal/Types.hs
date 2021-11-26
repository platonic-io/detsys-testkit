module Journal.Types where

import Control.Concurrent.STM (TVar)
import Data.ByteString (ByteString)
import Data.Word (Word32, Word64)

------------------------------------------------------------------------

newtype Bytes = Bytes Int

newtype BytesCounter = BytesCounter Word64

data Journal = Journal
  { jFile     :: {-# UNPACK #-} !(TVar FilePath)
  , jOffset   :: {-# UNPACK #-} !(TVar Word64) -- Tail.
  , jMaxSize  :: !Word64 -- XXX: unit? bytes? mb?
  , jSequence :: {-# UNPACK #-} !(TVar Word64)
  -- jPointerToActiveFile
  -- jGatingBytes :: IORef Word64
  , jMetrics :: Metrics
  }

data Metrics = Metrics
  { mAbortedConnections :: Word32
  , mReplaySize :: Int -- XXX: Histogram
  }

emptyMetrics :: Metrics
emptyMetrics = Metrics 0 0

data Options = Options
  -- buffer and fsync every ms?
  -- max disk space in total? multiple of maxSize?

newtype FileIndex  = FileIndex Word32
newtype ByteOffset = ByteOffset Word64

data Position = Position
  { pFileIndex  :: {-# UNPACK #-} !FileIndex
  , pByteOffset :: {-# UNPACK #-} !ByteOffset
  }
