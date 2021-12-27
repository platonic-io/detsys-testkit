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
  , align
  )
  where

import Control.Concurrent.STM
import Control.Concurrent.STM (TVar)
import Data.ByteString (ByteString)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.Bits
import Data.Vector (Vector)
import Data.Word (Word32, Word64, Word8)
import Foreign.Ptr (Ptr, plusPtr)
import Foreign.Storable (sizeOf)

import Journal.Internal.ByteBuffer
import Journal.Types.AtomicCounter

------------------------------------------------------------------------

-- XXX: `casIntArray` only works on `Int`, so we can't use `Int32` and `Int64`
-- yet. According to
-- https://gitlab.haskell.org/ghc/ghc/-/blob/master/libraries/ghc-prim/changelog.md#080-edit-as-necessary
-- casInt{32,64}Array should be part of ghc-prim 0.8.0, but the uploaded hackage
-- package's changelog says it isn't part of that release, nor do the haddocks
-- include it... Once this has been fixed we can remove the following type
-- aliases:
type Int64 = Int
type Int32 = Int

pARTITION_COUNT :: Int
pARTITION_COUNT = 3

data Journal' = Journal'
  { jTermBuffers   :: {-# UNPACK #-} !(Vector ByteBuffer)
  , jMetadata      :: {-# UNPACK #-} !ByteBuffer
  , jPositionLimit :: {-# UNPACK #-} !AtomicCounter -- ???
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
rawTail meta partitionIndex =
  readIntOffArrayIx meta
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
  writeIntOffArrayIx meta
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
activeTermCount meta = readIntOffArrayIx meta lOG_ACTIVE_TERM_COUNT_OFFSET

setActiveTermCount :: ByteBuffer -> Int32 -> IO ()
setActiveTermCount meta = writeIntOffArrayIx meta lOG_ACTIVE_TERM_COUNT_OFFSET

casActiveTermCount :: ByteBuffer -> Int32 -> Int32 -> IO Bool
casActiveTermCount meta expectedTermCount newTermCount =
  casIntArray meta lOG_ACTIVE_TERM_COUNT_OFFSET expectedTermCount newTermCount

initialTermId :: ByteBuffer -> IO Int32
initialTermId meta = readIntOffArrayIx meta lOG_INITIAL_TERM_ID_OFFSET

-- should never be changed?
-- setInitialTermId :: ByteBuffer -> Int32 -> IO ()
-- setInitialTermId meta = writeInt32OffArrayIx meta lOG_INITIAL_TERM_ID_OFFSET

termLength :: ByteBuffer -> IO Int32
termLength meta = readIntOffArrayIx meta lOG_TERM_LENGTH_OFFSET

-- should never be changed?
-- setTermLength :: ByteBuffer -> Int32 -> IO ()
-- setTermLength meta = writeInt32OffArrayIx meta lOG_TERM_LENGTH_OFFSET

pageSize :: ByteBuffer -> IO Int32
pageSize meta = readIntOffArrayIx meta lOG_PAGE_SIZE_OFFSET

-- | The number of bits to shift when multiplying or dividing by the term buffer
-- length.
positionBitsToShift :: Int32 -> Int
positionBitsToShift termBufferLength =
  case termBufferLength of
    65536      {- 64   * 1024 -}        -> 16
    131072     {- 128  * 1024 -}        -> 17
    262144     {- 256  * 1024 -}        -> 18
    524288     {- 512  * 1024 -}        -> 19
    1048576    {- 1024 * 1024 -}        -> 20
    2097152    {- 2    * 1024 * 1024 -} -> 21
    4194304    {- 4    * 1024 * 1024 -} -> 22
    8388608    {- 8    * 1024 * 1024 -} -> 23
    16777216   {- 16   * 1024 * 1024 -} -> 24
    33554432   {- 32   * 1024 * 1024 -} -> 25
    67108864   {- 64   * 1024 * 1024 -} -> 26
    134217728  {- 128  * 1024 * 1024 -} -> 27
    268435456  {- 256  * 1024 * 1024 -} -> 28
    536870912  {- 512  * 1024 * 1024 -} -> 29
    1073741824 {- 1024 * 1024 * 1024 -} -> 30
    _otherwise ->
      error ("positionBitsToShift: invalid term buffer length: " ++
             show (termBufferLength))

------------------------------------------------------------------------

-- | Rotate to the next partition in sequence for the current term id.
nextPartitionIndex :: Int32 -> Int32
nextPartitionIndex currentIndex =
  (currentIndex + 1) `mod` fromIntegral pARTITION_COUNT

-- | Calculate the partition index to be used given the initial term and active
-- term ids.
indexByTerm :: Int32 -> Int32 -> Int32
indexByTerm initTermId activeTermId =
  (activeTermId - initTermId) `mod` fromIntegral pARTITION_COUNT

-- | Caluclate the partition index based on number of terms that have passed.
indexByTermCount :: Int32 -> Int
indexByTermCount termCount = fromIntegral termCount `mod` pARTITION_COUNT

-- | Calculate the partition index given a stream position.
indexByPosition :: Int64 -> Int -> Int
indexByPosition pos posBitsToShift = fromIntegral $
  (pos `shiftR` posBitsToShift) `mod` fromIntegral pARTITION_COUNT

-- | Compute the current position in absolute number of bytes.
computePosition :: Int32 -> Int32 -> Int -> Int32 -> Int64
computePosition activeTermId termOffset posBitsToShift initTermId =
  let
    termCount :: Int64
    -- Copes with negative `activeTermId` on rollover.
    termCount = fromIntegral (activeTermId - initTermId)
  in
    (termCount `shiftL` posBitsToShift) + fromIntegral termOffset

-- | Compute the current position in absolute number of bytes for the beginning
-- of a term.
computeTermBeginPosition :: Int32 -> Int32 -> Int32 -> Int64
computeTermBeginPosition activeTermId posBitsToShift initTermId =
  let
    termCount :: Int64
    -- Copes with negative `activeTermId` on rollover.
    termCount = fromIntegral (activeTermId - initTermId)
  in
    termCount `shiftL` fromIntegral posBitsToShift

-- | Compute the term id from a position.
computeTermIdFromPosition :: Int64 -> Int -> Int32 -> Int32
computeTermIdFromPosition pos posBitsToShift initTermId = fromIntegral $
  (pos `shiftR` posBitsToShift) + fromIntegral initTermId

-- | Compute the total length of a log file given the term length.
computeLogLength :: Int -> Int -> Int64
computeLogLength termLen filePageSize
  | termLen < (1024 * 1024 * 1024) = fromIntegral $
      align ((termLen * pARTITION_COUNT) + lOG_META_DATA_LENGTH) filePageSize
  | otherwise = fromIntegral $
      (pARTITION_COUNT * termLen) + align lOG_META_DATA_LENGTH filePageSize

-- | Align a value to the next multiple up of alignment.
--
-- If the value equals an alignment multiple then it is returned unchanged.
--
-- This method executes without branching. This code is designed to be use in
-- the fast path and should not be used with negative numbers. Negative numbers
-- will result in undefined behaviour.
align :: Int -> Int -> Int
align value alignment = (value + (alignment - 1)) .&. (- alignment)

-- | Rotate the log and update the tail counter for the new term. This function
-- is thread safe.
rotateLog :: ByteBuffer -> Int32 -> Int32 -> IO Bool
rotateLog meta termCount termId0 = do
  go
  casActiveTermCount meta termCount nextTermCount
  where
    nextTermId     = termId0   + 1
    nextTermCount  = termCount + 1
    nextIndex      = indexByTermCount nextTermCount
    expectedTermId = nextTermId - fromIntegral pARTITION_COUNT

    go = do
      rt <- rawTail meta nextIndex
      if expectedTermId /= termId rt
      then return ()
      else do
        b <- casRawTail meta nextIndex rt (packTail nextTermId 0)
        if b then return () else go

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
