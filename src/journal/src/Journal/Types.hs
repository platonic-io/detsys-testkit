{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Journal.Types
--  ( Journal'(Journal')
--  , Journal(Journal)
--  , jMaxByteSize
--  , jOffset
--  , jDirectory
--  , jBytesConsumed
--  , Options(Options)
--  , oMaxByteSize
--  , oTermBufferLength
--  , JournalConsumer(JournalConsumer)
--  , jcBytesConsumed
--  , jcDirectory
--  , jcMaxByteSize
--  , newJournalPtrRef
--  , readJournalPtr
--  , updateJournalPtr
--  , newJournalConsumerPtrRef
--  , readJournalConsumerPtr
--  , updateJournalConsumerPtr
--  , getMaxByteSize
--  , readFileCount
--  , bumpFileCount
--  , module Journal.Types.AtomicCounter
--  , packTail
--  , termId
--  , termOffset
--  , align
--  )
  where

import Control.Concurrent.STM
import Control.Concurrent.STM (TVar)
import Data.Binary (Binary)
import Data.Bits
import Data.ByteString (ByteString)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.Vector (Vector)
import Data.Word (Word32, Word64, Word8)
import Foreign.Ptr (Ptr, plusPtr)
import Foreign.Storable (Storable, sizeOf)

import Journal.Internal.ByteBufferPtr
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

newtype Metadata = Metadata ByteBuffer

data Journal' = Journal'
  { jTermBuffers   :: {-# UNPACK #-} !(Vector ByteBuffer)
  , jMetadata      :: {-# UNPACK #-} !Metadata
  , jBytesConsumed :: {-# UNPACK #-} !AtomicCounter -- ???
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

newtype RawTail = RawTail { unRawTail :: Int64 }
  deriving newtype (Integral, Real, Num, Enum, Ord, Eq, Bits)

newtype PartitionIndex = PartitionIndex { unPartitionIndex :: Int }
  deriving newtype (Integral, Real, Num, Enum, Ord, Eq)

newtype TermId = TermId { unTermId :: Int32 }
  deriving newtype (Integral, Real, Num, Enum, Ord, Eq)

newtype TermOffset = TermOffset { unTermOffset :: Int32 }
  deriving newtype (Integral, Real, Num, Enum, Ord, Eq)

newtype TermCount = TermCount { unTermCount :: Int32 }
  deriving newtype (Integral, Real, Num, Enum, Ord, Eq)

readRawTail :: Metadata -> PartitionIndex -> IO RawTail
readRawTail (Metadata meta) (PartitionIndex partitionIndex) =
  RawTail <$>
    readIntOffArrayIx meta
      (tERM_TAIL_COUNTERS_OFFSET + (sizeOf (8 :: Int64) * partitionIndex))

rawTailTermId :: RawTail -> TermId
rawTailTermId = fromIntegral . (`shiftR` 32) . unRawTail

rawTailTermOffset :: RawTail -> Int64 -> TermOffset
rawTailTermOffset (RawTail rt) termLen =
  fromIntegral (min (rt .&. 0xFFFF_FFFF) termLen)

packTail :: TermId -> TermOffset -> RawTail
packTail termId0 termOffset0 =
  (fromIntegral termId0 `shiftL` 32) .|. (fromIntegral termOffset0 .&. 0xFFFF_FFFF);

writeRawTail :: Metadata -> TermId -> TermOffset -> PartitionIndex -> IO ()
writeRawTail (Metadata meta) termId termOffset (PartitionIndex partitionIndex) =
  writeIntOffAddr meta
    (tERM_TAIL_COUNTERS_OFFSET + (sizeOf (8 :: Int64) * partitionIndex))
    (unRawTail (packTail termId termOffset))

casRawTail :: Metadata -> PartitionIndex -> RawTail -> RawTail -> IO Bool
casRawTail (Metadata meta) (PartitionIndex partitionIndex) expected new =
  casIntAddr meta
    (tERM_TAIL_COUNTERS_OFFSET + (sizeOf (8 :: Int64) * partitionIndex))
    (fromIntegral expected) (fromIntegral new) -- XXX: 32-bit systems?

initialiseTailWithTermId :: Metadata -> PartitionIndex -> TermId -> IO ()
initialiseTailWithTermId meta partitionIndex termId =
  writeRawTail meta termId 0 partitionIndex

activeTermCount :: Metadata -> IO TermCount
activeTermCount (Metadata meta) =
  TermCount <$> readIntOffArrayIx meta lOG_ACTIVE_TERM_COUNT_OFFSET

writeActiveTermCount :: Metadata -> TermCount -> IO ()
writeActiveTermCount (Metadata meta) = do
  writeIntOffAddr meta lOG_ACTIVE_TERM_COUNT_OFFSET . fromIntegral

casActiveTermCount :: Metadata -> TermCount -> TermCount -> IO Bool
casActiveTermCount (Metadata meta) (TermCount expected) (TermCount new) =
  casIntAddr meta lOG_ACTIVE_TERM_COUNT_OFFSET expected new

initialTermId :: Metadata -> IO TermId
initialTermId (Metadata meta) =
  TermId <$> readIntOffArrayIx meta lOG_INITIAL_TERM_ID_OFFSET

-- should never be changed?
-- setInitialTermId :: ByteBuffer -> Int32 -> IO ()
-- setInitialTermId meta = writeInt32OffArrayIx meta lOG_INITIAL_TERM_ID_OFFSET

readTermLength :: Metadata -> IO Int32
readTermLength (Metadata meta) = readIntOffArrayIx meta lOG_TERM_LENGTH_OFFSET

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
nextPartitionIndex :: PartitionIndex -> PartitionIndex
nextPartitionIndex currentIndex =
  (currentIndex + 1) `mod` fromIntegral pARTITION_COUNT

-- | Calculate the partition index to be used given the initial term and active
-- term ids.
indexByTerm :: TermId -> TermId -> PartitionIndex
indexByTerm initTermId activeTermId = fromIntegral $
  (activeTermId - initTermId) `mod` fromIntegral pARTITION_COUNT

-- | Caluclate the partition index based on number of terms that have passed.
indexByTermCount :: TermCount -> PartitionIndex
indexByTermCount termCount = PartitionIndex $
  fromIntegral termCount `mod` pARTITION_COUNT

-- | Calculate the partition index given a stream position.
indexByPosition :: Int64 -> Int -> PartitionIndex
indexByPosition pos posBitsToShift = fromIntegral $
  (pos `shiftR` posBitsToShift) `mod` fromIntegral pARTITION_COUNT

-- | Compute the current position in absolute number of bytes.
computePosition :: TermId -> TermOffset -> Int -> TermId -> Int64
computePosition activeTermId termOffset posBitsToShift initTermId =
  computeTermBeginPosition activeTermId posBitsToShift initTermId + fromIntegral termOffset

-- | Compute the current position in absolute number of bytes for the beginning
-- of a term.
computeTermBeginPosition :: TermId -> Int32 -> TermId -> Int64
computeTermBeginPosition activeTermId posBitsToShift initTermId =
  let
    termCount :: Int64
    -- Copes with negative `activeTermId` on rollover.
    termCount = fromIntegral (activeTermId - initTermId)
  in
    termCount `shiftL` fromIntegral posBitsToShift

-- | Compute the term id from a position.
computeTermIdFromPosition :: Int64 -> Int -> TermId -> Int32
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
rotateLog :: Metadata -> TermCount -> TermId -> IO Bool
rotateLog meta termCount termId = do
  go
  casActiveTermCount meta termCount nextTermCount
  where
    nextTermId     = termId    + 1
    nextTermCount  = termCount + 1
    nextIndex      = indexByTermCount nextTermCount
    expectedTermId = nextTermId - fromIntegral pARTITION_COUNT

    go = do
      rawTail <- readRawTail meta nextIndex
      if expectedTermId /= rawTailTermId rawTail
      then return ()
      else do
        b <- casRawTail meta nextIndex rawTail (packTail nextTermId 0)
        if b then return () else go

------------------------------------------------------------------------

writeFrameType :: ByteBuffer -> TermOffset -> HeaderTag -> IO ()
writeFrameType termBuffer termOffset (HeaderTag tag) =
  putByteAt termBuffer (fromIntegral termOffset + tAG_FIELD_OFFSET) tag

writeFrameLength :: ByteBuffer -> TermOffset -> HeaderLength -> IO ()
writeFrameLength termBuffer termOffset (HeaderLength len) =
  writeWord32OffAddr termBuffer (fromIntegral termOffset + fRAME_LENGTH_FIELD_OFFSET)
    len

------------------------------------------------------------------------

newtype HeaderTag = HeaderTag { unHeaderTag :: Word8 }
  deriving newtype (Eq, Binary, Bits, Show, Num, Storable)

pattern Empty   = 0 :: HeaderTag
pattern Valid   = 1 :: HeaderTag
pattern Invalid = 2 :: HeaderTag
pattern Padding = 4 :: HeaderTag

tagString :: HeaderTag -> String
tagString Empty   = "Empty"
tagString Valid   = "Valid"
tagString Invalid = "Invalid"
tagString Padding = "Padding"
tagString other   = "Unknown: " ++ show other

newtype HeaderVersion = HeaderVersion Word8
  deriving newtype (Eq, Binary, Num, Storable, Integral, Real, Ord, Enum)

newtype HeaderLength = HeaderLength Word32
  deriving newtype (Eq, Ord, Binary, Enum, Real, Integral, Num, Storable)

newtype HeaderIndex = HeaderIndex Word32
  deriving newtype (Eq, Binary, Num, Storable)

------------------------------------------------------------------------

-- * Constants

-- | The length of the journal entry header in bytes.
hEADER_LENGTH :: Int
hEADER_LENGTH
  = sizeOf (1 :: HeaderTag)
  + sizeOf (1 :: HeaderVersion)
  + sizeOf (4 :: HeaderLength)
  -- + sizeOf (4 :: HeaderIndex)
  -- XXX: CRC?

fOOTER_LENGTH :: Int
fOOTER_LENGTH = hEADER_LENGTH

cURRENT_VERSION :: HeaderVersion
cURRENT_VERSION = 0

aCTIVE_FILE :: FilePath
aCTIVE_FILE = "active"

dIRTY_FILE :: FilePath
dIRTY_FILE = "dirty"

cLEAN_FILE :: FilePath
cLEAN_FILE = "clean"

sNAPSHOT_FILE :: FilePath
sNAPSHOT_FILE = "snapshot"

aRCHIVE_FILE :: FilePath
aRCHIVE_FILE = "archive"

fRAME_LENGTH_FIELD_OFFSET :: Int
fRAME_LENGTH_FIELD_OFFSET = 0

tAG_FIELD_OFFSET :: Int
tAG_FIELD_OFFSET = 6

------------------------------------------------------------------------

data Journal = Journal
  { jPtr           :: {-# UNPACK #-} !(TVar (Ptr Word8))
  , jOffset        :: {-# UNPACK #-} !AtomicCounter
  , jMaxByteSize   :: {-# UNPACK #-} !Int
  , jDirectory     ::                !FilePath
  , jBytesConsumed' :: {-# UNPACK #-} !AtomicCounter -- jGatingBytes?
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
  { oMaxByteSize :: !Int
  , oTermBufferLength :: !Int
  }
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
