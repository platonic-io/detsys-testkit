module Journal.MP where

import Control.Monad (when)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
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
       (ByteBuffer, getByteStringAt, getCapacity, unCapacity)
import Journal.Internal.Logger (Logger, logg)
import Journal.Internal.Utils
import Journal.Types
import Journal.Types.AtomicCounter

------------------------------------------------------------------------

appendBS :: Journal -> ByteString -> IO (Either AppendError ())
appendBS jour bs = do
  assertIO $ do
    termBufferLen <- int322Int <$> readTermLength (jMetadata jour)
    return (0 < BS.length bs && align (hEADER_LENGTH + BS.length bs) fRAME_ALIGNMENT <=
            termBufferLen `div` 2)
  let len = BS.length bs
  eClaim <- tryClaim jour len
  case eClaim of
    Left err -> return (Left err)
    Right (_offset, bufferClaim) -> do
      putBS bufferClaim hEADER_LENGTH bs
      Right <$> commit bufferClaim (jLogger jour)

readJournal :: Journal -> IO (Maybe ByteString)
readJournal jour = do
  offset <- readCounter (jBytesConsumed jour)
  let jLog = logg (jLogger jour)
  jLog ("readJournal, offset: " ++ show offset)

  termLen <- readTermLength (jMetadata jour)
  let readIndex = indexByPosition (int2Int64 offset) (positionBitsToShift termLen)
  jLog ("readJournal, readIndex: " ++ show (unPartitionIndex readIndex))

  termCount <- activeTermCount (jMetadata jour)
  let activeTermIndex = indexByTermCount termCount
  rawTail <- readRawTail (jMetadata jour) activeTermIndex
  let termBuffer = jTermBuffers jour Vector.! unPartitionIndex readIndex
      activeTermId = rawTailTermId rawTail
      termOffset = rawTailTermOffset rawTail termLen

  jLog ("readJournal, termOffset: " ++ show (unTermOffset termOffset))
  initTermId <- readInitialTermId (jMetadata jour)
  jLog ("readJournal, initTermId: " ++ show (unTermId initTermId))
  let position =
        computePosition activeTermId termOffset (positionBitsToShift termLen) initTermId
  -- putStrLn ("readJournal, offset: " ++ show offset ++ ", position: " ++ show position)
  -- assertM (int2Int64 offset <= position)

  let readTermCount =
        computeTermIdFromPosition (int2Int64 offset) (positionBitsToShift termLen) initTermId
        - unTermId initTermId

  jLog ("readJournal, readTermCount: " ++ show readTermCount)

  if int2Int64 offset == position
  then return Nothing
  else do
    -- assertM (int2Int64 offset < position)

    let relativeOffset = int2Int32 (align offset fRAME_ALIGNMENT) - readTermCount * termLen
    jLog ("readJournal, relativeOffset: " ++ show relativeOffset)
    tag <- readFrameType termBuffer (TermOffset relativeOffset)
    jLog ("readJournal, tag: " ++ show tag)
    HeaderLength len <- readFrameLength termBuffer (TermOffset relativeOffset)
    jLog ("readJournal, len: " ++ show len)
    if tag == Padding
    then do
      if len >= 0
      then do
        _success <- casCounter (jBytesConsumed jour) offset (offset + int322Int len)
        jLog "readJournal, skipping padding..."
        -- If the CAS fails, it just means that some other process incremented the
        -- counter already.
        readJournal jour
      else return Nothing -- If len is negative then the writer hasn't
                          -- finished writing the padding yet.
    else if len <= 0
         then return Nothing
         else do
           assertMMsg (show len) (len > 0)
           jLog ("readJournal, termCount: " ++ show (unTermCount termCount))
           success <- casCounter (jBytesConsumed jour) offset
                        (offset + (align (int322Int len) fRAME_ALIGNMENT))
           if success
           then do
             bs <- getByteStringAt termBuffer
                     (int322Int relativeOffset + hEADER_LENGTH)
                     (int322Int len - hEADER_LENGTH)
             assertM (BS.length bs == int322Int len - hEADER_LENGTH)
             -- Single-threaded case:
             -- incrCounter_ (align (int322Int len) fRAME_ALIGNMENT) (jBytesConsumed jour)
             return (Just bs)
           else
             -- If the CAS failed it means that another process read what we were
             -- about to read, so we retry reading the next item instead.
             readJournal jour

------------------------------------------------------------------------

tryClaim :: Journal -> Int -> IO (Either AppendError (Int64, BufferClaim))
tryClaim jour len = do
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
  else if position < limit
       then do
         eResult <- termAppenderClaim jour len termId
         newPosition (jMetadata jour) termCount termOffset termId position eResult
       else
         backPressureStatus position len (jLogger jour)

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

    go :: IO ()
    go = do
      rawTail <- readRawTail meta nextIndex
      if expectedTermId /= rawTailTermId rawTail
      then return ()
      else do
        b <- casRawTail meta nextIndex rawTail (packTail nextTermId 0)
        if b then return () else go

termAppenderClaim :: Journal -> Int -> TermId
                  -> IO (Either AppendError (TermOffset, BufferClaim))
termAppenderClaim jour len activeTermId = do
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
    handleEndOfLogCondition termBuffer termOffset termLen termId (jLogger jour)
    return (Left Rotation)
  else do
    let frameOffset = termOffset
    headerWrite termBuffer frameOffset (HeaderLength (int2Int32 frameLen)) termId
      (jLogger jour)
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
