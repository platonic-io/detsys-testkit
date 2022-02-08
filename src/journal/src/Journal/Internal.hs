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
import System.IO.MMap (Mode(ReadWriteEx), mmapFilePtr, munmapFilePtr)

import Journal.Internal.BufferClaim
import Journal.Internal.ByteBufferPtr
import Journal.Internal.Parse
import Journal.Internal.Utils
import Journal.Types
import Journal.Types.AtomicCounter

------------------------------------------------------------------------

offer :: Ptr Word8 -> Int -> Int -> IO Int
offer buf offset len = undefined

data AppendError = BackPressure | Rotation
  deriving (Eq, Show)

tryClaim :: Journal -> Int -> IO (Either AppendError (Int64, BufferClaim))
tryClaim jour len = do
  -- checkPayloadLength len
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

  putStrLn ("tryClaim, termCount: " ++ show (unTermCount termCount))
  putStrLn ("tryClaim, activePartitionIndex: " ++ show (unPartitionIndex activePartitionIndex))
  putStrLn ("tryClaim, termOffset: " ++ show (unTermOffset termOffset))
  putStrLn ("tryClaim, termBeginPosition: " ++ show termBeginPosition)
  limit <- calculatePositionLimit jour
  let termAppender = jTermBuffers jour Vector.! unPartitionIndex activePartitionIndex
      position     = termBeginPosition + fromIntegral termOffset

  putStrLn ("tryClaim, position: " ++ show position)
  putStrLn ("tryClaim, limit: " ++ show limit)
  if position < fromIntegral limit
  then do
    mResult <- termAppenderClaim (jMetadata jour) termAppender termId termOffset len
    newPosition (jMetadata jour) mResult
  else
    backPressureStatus position len

-- XXX: Save the result in `producerLimit :: AtomicCounter` and update it in a
-- separate process?
calculatePositionLimit :: Journal -> IO Int
calculatePositionLimit jour = do
  minSubscriberPos <- readCounter (jBytesConsumed jour) -- XXX: only one subscriber so far.
  maxSubscriberPos <- readCounter (jBytesConsumed jour)
  termWindowLen    <- termWindowLength (jMetadata jour)
  let _consumerPos  = maxSubscriberPos
      proposedLimit = minSubscriberPos + fromIntegral termWindowLen
  cleanBufferTo jour minSubscriberPos
  return proposedLimit
  where
    termWindowLength :: Metadata -> IO Int32
    termWindowLength meta = do
      termLen <- readTermLength meta
      return (termLen `shiftR` 1) -- / 2

cleanBufferTo :: Journal -> Int -> IO ()
cleanBufferTo _ _ = return ()

backPressureStatus :: Int64 -> Int -> IO (Either AppendError (Int64, BufferClaim))
backPressureStatus position len = do
  putStrLn ("backPressureStatus, position: " ++ show position ++ ", len: " ++ show len)
  return (Left BackPressure)

newPosition :: Metadata -> Either AppendError (TermOffset, BufferClaim)
            -> IO (Either AppendError (Int64, BufferClaim))
newPosition meta eResult =
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
      rotateTerm meta
      return (Left Rotation) -- ADMIN_ACTION

fRAME_ALIGNMENT :: Int
fRAME_ALIGNMENT = hEADER_LENGTH

termAppenderClaim :: Metadata -> ByteBuffer -> TermId -> TermOffset -> Int
                  -> IO (Either AppendError (TermOffset, BufferClaim))
termAppenderClaim meta termBuffer termId termOffset len = do
  let
    frameLength     = len + hEADER_LENGTH
    alignedLength   = align frameLength fRAME_ALIGNMENT
    resultingOffset = termOffset + fromIntegral alignedLength
    termLength      = getCapacity termBuffer
  termCount <- activeTermCount meta
  let activePartitionIndex = indexByTermCount termCount
  putStrLn ("termAppenderClaim, resultingOffset: " ++
            show (unTermOffset resultingOffset))
  writeRawTail meta termId resultingOffset activePartitionIndex
  if resultingOffset > TermOffset (int2Int32 (unCapacity termLength))
  then do
    handleEndOfLogCondition termBuffer termOffset termLength termId
    return (Left Rotation)
  else do
    putStrLn ("termAppenderClaim, termOffset: " ++
              show (unTermOffset termOffset))
    putStrLn ("termAppenderClaim, frameLength: " ++
              show frameLength)
    headerWrite termBuffer termOffset (fromIntegral frameLength) termId
    bufClaim <- newBufferClaim termBuffer termOffset frameLength
    return (Right (resultingOffset, bufClaim))

handleEndOfLogCondition :: ByteBuffer -> TermOffset -> Capacity -> TermId -> IO ()
handleEndOfLogCondition termBuffer termOffset (Capacity termLen) termId = do
  putStrLn "handleEndOfLogCondition"
  when (termOffset < fromIntegral termLen) $ do
    putStrLn "handleEndOfLogCondition: when"

    let paddingLength :: HeaderLength
        paddingLength = fromIntegral (termLen - fromIntegral termOffset)

    putStrLn ("handleEndOfLogCondition, paddingLength: " ++ show (unHeaderLength paddingLength))
    headerWrite termBuffer termOffset paddingLength termId
    putStrLn ("handleEndOfLogCondition, headerWrite")
    writeFrameType termBuffer termOffset Padding
    putStrLn ("handleEndOfLogCondition, writeFrameType: padding")
    writeFrameLength termBuffer termOffset paddingLength
    putStrLn ("handleEndOfLogCondition, writeFrameLength")

headerWrite :: ByteBuffer -> TermOffset -> HeaderLength -> TermId -> IO ()
headerWrite termBuffer termOffset len _termId = do
  let versionFlagsType :: Int64
      versionFlagsType = fromIntegral cURRENT_VERSION `shiftL` 32
  -- XXX: Atomic write?
  putStrLn ("headerWrite, versionFlagsType: " ++ show versionFlagsType)
  putStrLn ("headerWrite, len: " ++ show (- unHeaderLength len))
  putStrLn ("headerWrite, value: " ++ show
            (versionFlagsType .|. ((- int322Int64 (unHeaderLength len)) .&. 0xFFFF_FFFF)))
  writeInt64OffAddr termBuffer (fromIntegral termOffset + fRAME_LENGTH_FIELD_OFFSET)
     (versionFlagsType .|. ((- int322Int64 (unHeaderLength len)) .&. 0xFFFF_FFFF))
  -- XXX: store termId and offset (only need for replication?)

rotateTerm :: Metadata -> IO ()
rotateTerm meta = do
  termCount <- activeTermCount meta
  putStrLn ("rotateTerm, termCount: " ++ show (unTermCount termCount))
  let activePartitionIndex = indexByTermCount termCount
      nextIndex = nextPartitionIndex activePartitionIndex
  rawTail <- readRawTail meta activePartitionIndex
  initTermId <- readInitialTermId meta
  let termId = rawTailTermId rawTail

  assertM (termCount == TermCount (unTermId (termId - initTermId)))

  let termId' = termId + 1
      termCount' = TermCount (unTermId (termId' - initTermId))
  putStrLn ("rotateTerm, activePartitionIndex: " ++
            show (unPartitionIndex activePartitionIndex))
  putStrLn ("rotateTerm, initialTermId: " ++ show (unTermId initTermId))
  putStrLn ("rotateTerm, termId: " ++ show (unTermId termId))
  putStrLn ("rotateTerm, termId': " ++ show (unTermId termId'))
  putStrLn ("rotateTerm, termCount': " ++ show (unTermCount termCount'))


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
    bodyLength (_tag, HeaderLength len) = int322Int len - hEADER_LENGTH


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
