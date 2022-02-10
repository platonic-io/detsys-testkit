module Journal.MP where

import Control.Monad (when)
import Data.Int (Int64)
import qualified Data.Vector as Vector

import Journal.Internal
       ( AppendError(..)
       , backPressureStatus
       , calculatePositionLimit
       , fRAME_ALIGNMENT
       , headerWrite
       )
import Journal.Internal.BufferClaim
import Journal.Internal.ByteBufferPtr
       (ByteBuffer, getCapacity, unCapacity)
import Journal.Internal.Logger (Logger, logg)
import Journal.Internal.Utils
import Journal.Types

------------------------------------------------------------------------

tryClaim :: Journal -> Int -> Logger -> IO (Either AppendError (Int64, BufferClaim))
tryClaim jour len logger = do
  -- XXX: checkPayloadLength len

  limit <- calculatePositionLimit jour
  termCount <- activeTermCount (jMetadata jour)
  let activePartitionIndex = indexByTermCount termCount
      termBuffer = jTermBuffers jour Vector.! unPartitionIndex activePartitionIndex
  rawTail <- readRawTail (jMetadata jour) activePartitionIndex
  initTermId <- readInitialTermId (jMetadata jour)
  let termLen    = unCapacity (getCapacity termBuffer)
      termId     = rawTailTermId rawTail
      termOffset = rawTailTermOffset rawTail (int2Int32 termLen)
      position   =
        computeTermBeginPosition termId (positionBitsToShift (int2Int32 termLen)) initTermId
          + int322Int64 (unTermOffset termOffset)

  if unTermCount termCount /= unTermId (termId - initTermId)
  then return (Left AdminAction) -- XXX: what does this mean to end up here?
  else if position < int2Int64 limit
       then do
         eResult <- termAppenderClaim jour len termId logger
         newPosition (jMetadata jour) termCount termOffset termId position eResult
       else
         backPressureStatus position len logger

newPosition :: Metadata -> TermCount -> TermOffset -> TermId -> Int64
            -> Either AppendError (TermOffset, BufferClaim)
            -> IO (Either AppendError (Int64, BufferClaim))
newPosition meta termCount (TermOffset termOffset) termId position eResult =
  case eResult of
    Left Rotation -> do
      rotateLog meta termCount termId
      return (Left Rotation)
    Right (TermOffset resultingOffset, bufClaim) ->
      return (Right ((position - int322Int64 termOffset) + int322Int64 resultingOffset,
                     bufClaim))
  -- ^ XXX: when is this needed?
  -- | (position + termOffset) > maxPossiblePosition = return mAX_POSITION_EXCEEDED

termAppenderClaim :: Journal -> Int -> TermId -> Logger
                  -> IO (Either AppendError (TermOffset, BufferClaim))
termAppenderClaim jour len activeTermId logger = do
  let frameLen   = len + hEADER_LENGTH
      alignedLen = align frameLen fRAME_ALIGNMENT
  termCount <- activeTermCount (jMetadata jour)
  let activePartitionIndex = indexByTermCount termCount
  rawTail <- getAndAddRawTail (jMetadata jour) activePartitionIndex (int2Int64 alignedLen)

  let termBuffer = jTermBuffers jour Vector.! unPartitionIndex activePartitionIndex
      termLen    = unCapacity (getCapacity termBuffer)
      termId     = rawTailTermId rawTail
      termOffset = rawTailTermOffset rawTail (int2Int32 termLen)

  -- XXX:
  -- checkTerm activeTermId termId

  let resultingOffset = TermOffset (unTermOffset termOffset + int2Int32 alignedLen)

  if int322Int (unTermOffset resultingOffset) > termLen
  then do
    handleEndOfLogCondition termBuffer termOffset termLen termId logger
    return (Left Rotation)
  else do
    let frameOffset = termOffset
    headerWrite termBuffer frameOffset (HeaderLength (int2Int32 frameLen)) termId logger
    bufClaim <- newBufferClaim termBuffer termOffset frameLen
    return (Right (resultingOffset, bufClaim))

handleEndOfLogCondition :: ByteBuffer -> TermOffset -> Int -> TermId -> Logger -> IO ()
handleEndOfLogCondition termBuffer termOffset termLen termId logger =
  -- The first writer that went past the term buffer capacity writes the padding
  -- (later writes will all have `termOffset` higher than `termLen`).
  when (termOffset < fromIntegral termLen) $ do

    let paddingLength :: HeaderLength
        paddingLength = fromIntegral (termLen - fromIntegral termOffset)

    headerWrite termBuffer termOffset paddingLength termId logger
    writeFrameType termBuffer termOffset Padding
    writeFrameLength termBuffer termOffset paddingLength
