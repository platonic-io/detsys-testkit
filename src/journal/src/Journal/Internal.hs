{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NumericUnderscores #-}

module Journal.Internal where

import Control.Arrow ((&&&))
import Control.Concurrent (threadDelay)
import Control.Concurrent.STM (atomically, writeTVar)
import Control.Exception (assert, bracket)
import Control.Monad (unless, when)
import Data.Binary (Binary, decode, encode)
import Data.Bits (Bits, shiftL, shiftR, (.&.), (.|.))
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSChar8
import Data.ByteString.Internal (fromForeignPtr)
import qualified Data.ByteString.Lazy as LBS
import Data.Int (Int32, Int64)
import Data.List (isPrefixOf)
import Data.Maybe (catMaybes)
import qualified Data.Vector as Vector
import Data.Word (Word32, Word8)
import Foreign.ForeignPtr (newForeignPtr_)
import Foreign.Ptr (Ptr, plusPtr)
import Foreign.Storable (Storable, peekByteOff, pokeByteOff, sizeOf)
import System.Directory
       (copyFile, doesFileExist, listDirectory, renameFile)
import System.FilePath ((</>))

import Journal.Internal.BufferClaim
import Journal.Internal.ByteBufferPtr
import Journal.Internal.Logger (Logger, logg)
import Journal.Internal.Parse
import Journal.Internal.Utils
import Journal.Types
import Journal.Types.AtomicCounter

------------------------------------------------------------------------

offer :: Ptr Word8 -> Int -> Int -> IO Int
offer buf offset len = undefined

data AppendError = BackPressure | Rotation | AdminAction
  deriving (Eq, Show)

tryClaim :: Journal -> Int -> IO (Either AppendError (Int64, BufferClaim))
tryClaim jour len = do
  -- XXX:
  -- checkPayloadLength len -- This should use mdMTULength
  termCount <- activeTermCount (jMetadata jour)
  let index                = indexByTermCount termCount
      activePartitionIndex = index
  rt <- readRawTail (jMetadata jour) index
  initTermId <- readInitialTermId (jMetadata jour)
  termLen <- readTermLength (jMetadata jour)

  -- XXX: cache and read these from there?
  let termId            = rawTailTermId rt
      termOffset        = rawTailTermOffset rt termLen
      termBeginPosition =
        computeTermBeginPosition termId (positionBitsToShift termLen) initTermId
      jLog = logg (jLogger jour)

  jLog ("tryClaim, termCount: " ++ show (unTermCount termCount))
  jLog ("tryClaim, activePartitionIndex: " ++ show (unPartitionIndex activePartitionIndex))
  jLog ("tryClaim, termOffset: " ++ show (unTermOffset termOffset))
  jLog ("tryClaim, termBeginPosition: " ++ show termBeginPosition)
  limit <- calculatePositionLimit jour
  let termAppender = jTermBuffers jour Vector.! unPartitionIndex activePartitionIndex
      position     = termBeginPosition + fromIntegral termOffset

  jLog ("tryClaim, position: " ++ show position)
  jLog ("tryClaim, limit: " ++ show limit)
  if position < limit
  then do
    mResult <- exclusiveTermAppenderClaim (jMetadata jour) termAppender termId
                 termOffset len (jLogger jour)
    newPosition (jMetadata jour) mResult (jLogger jour)
  else
    backPressureStatus position len (jLogger jour)

-- XXX: Save the result in `producerLimit :: AtomicCounter` and update it in a
-- separate process?
calculatePositionLimit :: Journal -> IO Int64
calculatePositionLimit jour = do
  minSubscriberPos <- readCounter (jBytesConsumed jour) -- XXX: only one subscriber so far.
  maxSubscriberPos <- readCounter (jBytesConsumed jour)
  termWindowLen    <- termWindowLength (jMetadata jour)
  let _consumerPos  = maxSubscriberPos
      proposedLimit = minSubscriberPos + fromIntegral termWindowLen
  cleanBufferTo jour minSubscriberPos
  return (int2Int64 proposedLimit)
  where
    termWindowLength :: Metadata -> IO Int32
    termWindowLength meta = do
      termLen <- readTermLength meta
      return (termLen `shiftR` 1)
      -- ^ prop> \(Positive (Small i)) -> (2^i) `shiftR` 1 == 2^i `div` 2

cleanBufferTo :: Journal -> Int -> IO ()
cleanBufferTo jour position = do
  cleanPosition <- readCounter (jCleanPosition jour)
  termBufferLen <- readTermLength (jMetadata jour)
  when (position > cleanPosition) $ do
    let index = indexByPosition (int2Int64 cleanPosition) (positionBitsToShift termBufferLen)
        dirtyTerm = jTermBuffers jour Vector.! unPartitionIndex index
        bytesForCleaning = position - cleanPosition
        bufferCapacity = int322Int termBufferLen
        termOffset = cleanPosition .&. (bufferCapacity - 1)
        len = min bytesForCleaning (bufferCapacity - termOffset)
    cleanAt dirtyTerm (termOffset + sizeOf (8 :: Int64)) (len - sizeOf (8 :: Int64))
    writeInt64OffAddr dirtyTerm termOffset 0
    incrCounter_ len (jCleanPosition jour)

backPressureStatus :: Int64 -> Int -> Logger -> IO (Either AppendError (Int64, BufferClaim))
backPressureStatus position len logger = do
  logg logger ("backPressureStatus, position: " ++ show position ++ ", len: " ++ show len)
  return (Left BackPressure)

newPosition :: Metadata -> Either AppendError (TermOffset, BufferClaim) -> Logger
            -> IO (Either AppendError (Int64, BufferClaim))
newPosition meta eResult logger =
  case eResult of
    Right (resultingOffset, bufClaim) -> do
      -- XXX: cache
      -- termOffset := resultingOffset
      termCount <- activeTermCount meta
      let index = indexByTermCount termCount
      rt <- readRawTail meta index
      initTermId <- readInitialTermId meta
      termLen <- readTermLength meta

      let termId            = rawTailTermId rt
          termOffset        = rawTailTermOffset rt termLen
          termBeginPosition =
            computeTermBeginPosition termId (positionBitsToShift termLen) initTermId
      return (Right (termBeginPosition + fromIntegral resultingOffset, bufClaim))
    Left Rotation -> do
      -- XXX:
      -- if termBeginPosition + termBufferLength >= maxPossiblePosition
      -- then return Nothing -- return MAX_POSSILBE_POSITION_EXCEEDED ?
      -- else do
      rotateTerm meta logger
      return (Left Rotation) -- ADMIN_ACTION

fRAME_ALIGNMENT :: Int
fRAME_ALIGNMENT = hEADER_LENGTH

exclusiveTermAppenderClaim :: Metadata -> ByteBuffer -> TermId -> TermOffset -> Int -> Logger
                           -> IO (Either AppendError (TermOffset, BufferClaim))
exclusiveTermAppenderClaim meta termBuffer termId termOffset len logger = do
  let
    frameLength     = len + hEADER_LENGTH
    alignedLength   = align frameLength fRAME_ALIGNMENT
    resultingOffset = termOffset + fromIntegral alignedLength
    termLength      = getCapacity termBuffer
  termCount <- activeTermCount meta
  let activePartitionIndex = indexByTermCount termCount
      jLog = logg logger
  jLog ("termAppenderClaim, resultingOffset: " ++
            show (unTermOffset resultingOffset))
  writeRawTail meta termId resultingOffset activePartitionIndex
  if resultingOffset > TermOffset (int2Int32 (unCapacity termLength))
  then do
    handleEndOfLogCondition termBuffer termOffset termLength termId logger
    return (Left Rotation)
  else do
    jLog ("termAppenderClaim, termOffset: " ++ show (unTermOffset termOffset))
    jLog ("termAppenderClaim, frameLength: " ++ show frameLength)
    headerWrite termBuffer termOffset (fromIntegral frameLength) termId logger
    bufClaim <- newBufferClaim termBuffer termOffset frameLength
    return (Right (resultingOffset, bufClaim))

handleEndOfLogCondition :: ByteBuffer -> TermOffset -> Capacity -> TermId -> Logger -> IO ()
handleEndOfLogCondition termBuffer termOffset (Capacity termLen) termId logger = do
  let jLog = logg logger
  jLog "handleEndOfLogCondition"
  when (termOffset < fromIntegral termLen) $ do
    jLog "handleEndOfLogCondition: when"

    let paddingLength :: HeaderLength
        paddingLength = fromIntegral (termLen - fromIntegral termOffset)

    jLog ("handleEndOfLogCondition, paddingLength: " ++ show (unHeaderLength paddingLength))
    headerWrite termBuffer termOffset paddingLength termId logger
    jLog ("handleEndOfLogCondition, headerWrite")
    writeFrameType termBuffer termOffset Padding
    jLog ("handleEndOfLogCondition, writeFrameType: padding")
    writeFrameLength termBuffer termOffset paddingLength
    jLog ("handleEndOfLogCondition, writeFrameLength")

headerWrite :: ByteBuffer -> TermOffset -> HeaderLength -> TermId -> Logger -> IO ()
headerWrite termBuffer termOffset len _termId logger = do
  let versionFlagsType :: Int64
      versionFlagsType = fromIntegral cURRENT_VERSION `shiftL` 32
      jLog = logg logger
  -- XXX: Atomic write?
  jLog ("headerWrite, versionFlagsType: " ++ show versionFlagsType)
  jLog ("headerWrite, len: " ++ show (- unHeaderLength len))
  jLog ("headerWrite, value: " ++ show
            (versionFlagsType .|. ((- int322Int64 (unHeaderLength len)) .&. 0xFFFF_FFFF)))
  writeInt64OffAddr termBuffer (fromIntegral termOffset + fRAME_LENGTH_FIELD_OFFSET)
     (versionFlagsType .|. ((- int322Int64 (unHeaderLength len)) .&. 0xFFFF_FFFF))
  -- XXX: store termId and offset (only need for replication?)

rotateTerm :: Metadata -> Logger -> IO ()
rotateTerm meta logger = do
  termCount <- activeTermCount meta
  let jLog = logg logger
  jLog ("rotateTerm, termCount: " ++ show (unTermCount termCount))
  let activePartitionIndex = indexByTermCount termCount
      nextIndex = nextPartitionIndex activePartitionIndex
  rawTail <- readRawTail meta activePartitionIndex
  initTermId <- readInitialTermId meta
  let termId = rawTailTermId rawTail

  assertM (termCount == TermCount (unTermId (termId - initTermId)))

  let termId' = termId + 1
      termCount' = TermCount (unTermId (termId' - initTermId))
  jLog ("rotateTerm, activePartitionIndex: " ++
            show (unPartitionIndex activePartitionIndex))
  jLog ("rotateTerm, initialTermId: " ++ show (unTermId initTermId))
  jLog ("rotateTerm, termId: " ++ show (unTermId termId))
  jLog ("rotateTerm, termId': " ++ show (unTermId termId'))
  jLog ("rotateTerm, termCount': " ++ show (unTermCount termCount'))


  -- XXX: cache this? where exactly?
  -- activePartionIndex := nextIndex
  -- termOffset := 0
  -- termId := nextTermId
  -- termBeginPosition += termBufferLength

  initialiseTailWithTermId meta nextIndex termId'
  writeActiveTermCount meta termCount'

------------------------------------------------------------------------

type JournalHeader = JournalHeaderV0

data JournalHeaderV0 = JournalHeaderV0
  { jhTag      :: !HeaderTag
  , jhVersion  :: !HeaderVersion
  , jhLength   :: !HeaderLength
  -- , jhChecksum :: !Word32 -- V1
  }

------------------------------------------------------------------------

data Inconsistency
  = ActiveFileSizeMismatch Int Int
  | ActiveFileParseError String
  | PartialReceived
  | PartialRotation
  deriving Show

inconsistencyString :: Inconsistency -> String
inconsistencyString = show

inconsistenciesString :: [Inconsistency] -> String
inconsistenciesString = show . map inconsistencyString

checkForInconsistencies :: Journal -> IO [Inconsistency]
checkForInconsistencies jour = catMaybes <$> sequence
  []

fixInconsistency :: Inconsistency -> Journal -> IO ()
fixInconsistency = undefined

------------------------------------------------------------------------

-- * Debugging

dumpTermBuffer :: Int -> (ByteBuffer, TermOffset) -> IO ()
dumpTermBuffer i (bb, termOffset) = do
  putStrLn ("TermBuffer " ++ show i ++ ":")
  dumpEntries 0
  where
    dumpEntries :: TermOffset -> IO ()
    dumpEntries offset
      | offset == termOffset = return ()
      | offset >  termOffset = __IMPOSSIBLE__
      | offset <  termOffset = do
        h <- readHeader offset
        dumpHeader h
        dumpBody offset (bodyLength h)
        dumpEntries (offset +
                     TermOffset (int2Int32 (align (hEADER_LENGTH + bodyLength h)
                                            fRAME_ALIGNMENT)))

    readHeader :: TermOffset -> IO (HeaderTag, HeaderLength)
    readHeader offset = (,) <$> readFrameType bb offset <*> readFrameLength bb offset

    dumpHeader :: (HeaderTag, HeaderLength) -> IO ()
    dumpHeader (HeaderTag tag, HeaderLength len) = do
      putStrLn ("headerTag: " ++ show tag)
      putStrLn ("headerLength: " ++ show len)

    dumpBody :: TermOffset -> Int -> IO ()
    dumpBody offset len = do
      bs <- getByteStringAt bb (int322Int (unTermOffset offset) + hEADER_LENGTH) len
      putStr "body: "
      case encodeRunLength bs of
        [(n, c)]   -> putStrLn (show n ++ "x" ++ show c)
        _otherwise -> BSChar8.putStrLn bs

    encodeRunLength :: BS.ByteString -> [(Int, Char)]
    encodeRunLength = map (BSChar8.length &&& BSChar8.head) . BSChar8.group

    bodyLength :: (HeaderTag, HeaderLength) -> Int
    bodyLength (_tag, HeaderLength len) = max 0 (int322Int len - hEADER_LENGTH)


dumpMetadata :: Metadata -> IO ()
dumpMetadata meta = do
  putStrLn "Metadata"
  putStrLn "========"
  flip mapM_ [0 .. pARTITION_COUNT - 1] $ \i -> do
    RawTail rawTail <- readRawTail meta (PartitionIndex i)
    putStrLn ("rawTail" ++ show i ++ ": " ++ show rawTail)

  termCount <- activeTermCount meta
  putStrLn ("activeTermCount: " ++ show (unTermCount termCount))

  initTermId <- readInitialTermId meta
  putStrLn ("initialTermId: " ++ show (unTermId initTermId))

  termLen <- readTermLength meta
  putStrLn ("termBufferLength: " ++ show termLen)

  pageSize <- readPageSize meta
  putStrLn ("pageSize: " ++ show pageSize)

  putStrLn "--------"

  let index                = indexByTermCount termCount
      activePartitionIndex = index
  rawTail <- readRawTail meta index
  let termId            = rawTailTermId rawTail
      termOffset        = rawTailTermOffset rawTail termLen
      termBeginPosition =
        computeTermBeginPosition termId (positionBitsToShift termLen) initTermId

  putStrLn ("termId: " ++ show (unTermId termId))
  putStrLn ("termOffset: " ++ show (unTermOffset termOffset))
  putStrLn ("termBeginPosition: " ++ show termBeginPosition)
  putStrLn ("activePartitionIndex: " ++ show (unPartitionIndex activePartitionIndex))
