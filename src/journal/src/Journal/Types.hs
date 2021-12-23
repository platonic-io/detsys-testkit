{-# LANGUAGE NumericUnderscores #-}

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
  , module Journal.Types.AtomicCounter
  , packTail
  , termId
  , termOffset
  )
  where

import Control.Concurrent.STM
import Control.Concurrent.STM (TVar)
import Data.ByteString (ByteString)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.Int
import Data.Bits
import Data.Vector (Vector)
import Data.Word (Word32, Word64, Word8)
import Foreign.Ptr (Ptr, plusPtr)
import Foreign.Storable (sizeOf)

import Journal.Internal.ByteBuffer
import Journal.Types.AtomicCounter

------------------------------------------------------------------------

pARTITION_COUNT :: Int
pARTITION_COUNT = 3

data Journal' = Journal'
  { jTermBuffers :: {-# UNPACK #-} !(Vector ByteBuffer)
  , jMetadata    :: {-# UNPACK #-} !ByteBuffer
  }

data JMetadata = JMetadata
  { mdRawTail0      :: Int64
  , mdRawTail1      :: Int64
  , mdRawTail2      :: Int64
  , mdActiveCount   :: Int32
  -- padding
  , mdInitialTermId :: Int32
  -- mdDefaultFrameHeaderLength :: Int32?
  -- mdMTULength :: Int32, only needed if we want to fragment large messages...
  , mdTermLength    :: Int32
  , mdPageSize      :: Int32
  -- padding
  -- , mdDefaultFrameHeader :: Bytestring???
  }

tERM_TAIL_COUNTERS_OFFSET :: Int
tERM_TAIL_COUNTERS_OFFSET = 0

lOG_ACTIVE_TERM_COUNT_OFFSET :: Int
lOG_ACTIVE_TERM_COUNT_OFFSET = tERM_TAIL_COUNTERS_OFFSET +
  sizeOf (8 :: Int64) * pARTITION_COUNT

lOG_INITIAL_TERM_ID_OFFSET :: Int
lOG_INITIAL_TERM_ID_OFFSET = lOG_ACTIVE_TERM_COUNT_OFFSET +
  sizeOf (4 :: Int32)

lOG_TERM_LENGTH_OFFSET :: Int
lOG_TERM_LENGTH_OFFSET = lOG_INITIAL_TERM_ID_OFFSET +
  sizeOf (4 :: Int32)

lOG_PAGE_SIZE_OFFSET :: Int
lOG_PAGE_SIZE_OFFSET = lOG_TERM_LENGTH_OFFSET +
  sizeOf (4 :: Int32)

lOG_META_DATA_LENGTH :: Int
lOG_META_DATA_LENGTH = lOG_PAGE_SIZE_OFFSET

------------------------------------------------------------------------

rawTail :: ByteBuffer -> Int -> IO Int64
rawTail metadataBuffer partitionIndex =
  readInt64OffArrayIx metadataBuffer
    (tERM_TAIL_COUNTERS_OFFSET + (sizeOf (8 :: Int64) * partitionIndex))

termId :: Int64 -> Int32
termId = fromIntegral . (`shiftR` 32)

termOffset :: Int64 -> Int64 -> Int32
termOffset rawTail0 termLen =
  fromIntegral (min (rawTail0 .&. 0xFFFF_FFFF) termLen)

packTail :: Int32 -> Int32 -> Int64
packTail termId0 termOffset0 =
  (fromIntegral termId0 `shiftL` 32) .|. (fromIntegral termOffset0 .&. 0xFFFF_FFFF);

setRawTail :: ByteBuffer -> Int32 -> Int32 -> Int -> IO ()
setRawTail meta termId0 termOffset0 partitionIndex =
  writeInt64OffArrayIx meta
    (tERM_TAIL_COUNTERS_OFFSET + (sizeOf (8 :: Int64) * partitionIndex))
    (packTail termId0 termOffset0)

casRawTail :: ByteBuffer -> Int -> Int64 -> Int64 -> IO Bool
casRawTail meta partitionIndex expectedRawTail newRawTail =
  casIntArray meta
    (tERM_TAIL_COUNTERS_OFFSET + (sizeOf (8 :: Int64) * partitionIndex))
    (fromIntegral expectedRawTail) (fromIntegral newRawTail) -- XXX: 32-bit systems?

initialiseTailWithTermId :: ByteBuffer -> Int -> Int32 -> IO ()
initialiseTailWithTermId meta partitionIndex termId0 =
  setRawTail meta termId0 0 partitionIndex

activeTermCount :: ByteBuffer -> IO Int32
activeTermCount meta = readInt32OffArrayIx meta lOG_ACTIVE_TERM_COUNT_OFFSET

setActiveTermCount :: ByteBuffer -> Int32 -> IO ()
setActiveTermCount meta = writeInt32OffArrayIx meta lOG_ACTIVE_TERM_COUNT_OFFSET

casActiveTermCount :: ByteBuffer -> Int32 -> Int32 -> IO Bool
casActiveTermCount meta expectedTermCount newTermCount =
  undefined
  -- casIntArray only works on `Int`, does it mean we need to change all `Int32`
  -- to `Int`? Or can we keep `Int32` and use casIntArray + fromIntegral?

  -- casIntArray meta lOG_ACTIVE_TERM_COUNT_OFFSET expectedTermCount newTermCount

initialTermId :: ByteBuffer -> IO Int32
initialTermId meta = readInt32OffArrayIx meta lOG_INITIAL_TERM_ID_OFFSET

-- should never be changed?
-- setInitialTermId :: ByteBuffer -> Int32 -> IO ()
-- setInitialTermId meta = writeInt32OffArrayIx meta lOG_INITIAL_TERM_ID_OFFSET

termLength :: ByteBuffer -> IO Int32
termLength meta = readInt32OffArrayIx meta lOG_TERM_LENGTH_OFFSET

-- should never be changed?
-- setTermLength :: ByteBuffer -> Int32 -> IO ()
-- setTermLength meta = writeInt32OffArrayIx meta lOG_TERM_LENGTH_OFFSET

pageSize :: ByteBuffer -> IO Int32
pageSize meta = readInt32OffArrayIx meta lOG_PAGE_SIZE_OFFSET

------------------------------------------------------------------------

indexByTermCount :: Int32 -> Int
indexByTermCount termCount = fromIntegral termCount `mod` pARTITION_COUNT

computeTermBeginPosition :: Int32 -> Int32 -> Int32 -> Int64
computeTermBeginPosition activeTermId posBitsToShift initTermId =
  let
    termCount :: Int64
    -- Copes with negative `activeTermId` on rollover.
    termCount = fromIntegral (activeTermId - initTermId)
  in
    termCount `shiftL` fromIntegral posBitsToShift

rotateLog :: ByteBuffer -> Int32 -> Int32 -> IO Bool
rotateLog meta termCount termId0 = do
  -- XXX:
  undefined
  casActiveTermCount meta termCount (termCount + 1)

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
  -- page size? (for prefetching (see ghc-prim) and buffering writes?)

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
