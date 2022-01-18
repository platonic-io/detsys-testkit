{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NumericUnderscores #-}

module Journal.Internal where

import Control.Concurrent (threadDelay)
import Control.Concurrent.STM (atomically, writeTVar)
import Control.Exception (assert, bracket)
import Control.Monad (unless, when)
import Data.Binary (Binary, decode, encode)
import Data.Bits (Bits, shiftL, shiftR, (.&.), (.|.))
import qualified Data.ByteString as BS
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

tryClaim :: Journal -> Int -> IO (Maybe (Int64, BufferClaim))
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

  putStrLn ("tryClaim, termOffset: " ++ show (unTermOffset termOffset))
  limit <- calculatePositionLimit jour
  let termAppender = jTermBuffers jour Vector.! unPartitionIndex activePartitionIndex
      position     = termBeginPosition + fromIntegral termOffset

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

backPressureStatus _ _ = return Nothing

newPosition :: Metadata -> Maybe (TermOffset, BufferClaim) -> IO (Maybe (Int64, BufferClaim))
newPosition meta mResult =
  case mResult of
    Just (resultingOffset, bufClaim) -> do
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
      return (Just (termBeginPosition + fromIntegral resultingOffset, bufClaim))
    Nothing -> do
      -- XXX:
      -- if termBeginPosition + termBufferLength >= maxPossiblePosition
      -- then return Nothing -- return MAX_POSSILBE_POSITION_EXCEEDED ?
      -- else do
      rotateTerm meta
      return Nothing -- ADMIN_ACTION

fRAME_ALIGNMENT :: Int
fRAME_ALIGNMENT = 32

termAppenderClaim :: Metadata -> ByteBuffer -> TermId -> TermOffset -> Int
                  -> IO (Maybe (TermOffset, BufferClaim))
termAppenderClaim meta termBuffer termId termOffset len = do
  let
    frameLength     = len + hEADER_LENGTH
    alignedLength   = frameLength -- XXX: align frameLength fRAME_ALIGNMENT ?
    resultingOffset = termOffset + fromIntegral alignedLength
    termLength      = getCapacity termBuffer
  termCount <- activeTermCount meta
  let activePartitionIndex = indexByTermCount termCount
  putStrLn ("termAppenderClaim, resultingOffset: " ++
            show (unTermOffset resultingOffset))
  writeRawTail meta termId resultingOffset activePartitionIndex
  if resultingOffset > fromIntegral termLength
  then do
    handleEndOfLogCondition termBuffer termOffset termLength termId
    return Nothing
  else do
    putStrLn ("termAppenderClaim, termOffset: " ++
              show (unTermOffset termOffset))
    putStrLn ("termAppenderClaim, frameLength: " ++
              show frameLength)
    headerWrite termBuffer termOffset (fromIntegral frameLength) termId
    bufClaim <- newBufferClaim termBuffer termOffset frameLength
    return (Just (resultingOffset, bufClaim))

handleEndOfLogCondition :: ByteBuffer -> TermOffset -> Capacity -> TermId -> IO ()
handleEndOfLogCondition termBuffer termOffset (Capacity termLen) termId = do
  when (termOffset < fromIntegral termLen) $ do

    let paddingLength :: HeaderLength
        paddingLength = fromIntegral (termLen - fromIntegral termOffset)

    headerWrite termBuffer termOffset paddingLength termId
    writeFrameType termBuffer termOffset Padding
    writeFrameLength termBuffer termOffset paddingLength

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
  let activePartitionIndex = indexByTermCount termCount
      nextIndex = nextPartitionIndex activePartitionIndex
  rawTail <- readRawTail meta activePartitionIndex
  initTermId <- readInitialTermId meta
  let termId = rawTailTermId rawTail
      nextTermId = termId + 1
      termCount = fromIntegral (nextTermId - initTermId)

  -- XXX: cache this? where exactly?
  -- activePartionIndex := nextIndex
  -- termOffset := 0
  -- termId := nextTermId
  -- termBeginPosition += termBufferLength

  initialiseTailWithTermId meta nextIndex nextTermId
  writeActiveTermCount meta termCount

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

dumpTermBuffer :: ByteBuffer -> IO ()
dumpTermBuffer bb = return ()

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
