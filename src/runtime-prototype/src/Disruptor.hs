{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Disruptor where

import Data.Atomics (casIORef, readForCAS, peekTicket)
import Data.Bits
import Control.Concurrent
import Control.Concurrent.Async
import Control.Exception
import Control.Monad
import Data.IORef
import Data.Word
import Data.Vector.Mutable (IOVector)
import qualified Data.Vector.Mutable as Vector
import System.IO
import System.IO.Error
import System.Posix.Files

------------------------------------------------------------------------

newtype SequenceNumber = SequenceNumber { getSequenceNumber :: Word64 }
  deriving (Num, Eq, Ord, Real, Enum, Integral, Show, Bounded)

-- * Ring-buffer

data RingBufferMode = SingleProducer | MultiProducer

data RingBuffer e = RingBuffer
  { rbMode            :: RingBufferMode
  , rbCapacity        :: Word64
  , rbSequenceNumber  :: IORef SequenceNumber
  , rbEvents          :: IOVector e
  , rbGatingSequences :: IORef [IORef SequenceNumber] -- ^ References to the
                                                      -- last consumers'
                                                      -- sequence numbers, used
                                                      -- in order to avoid
                                                      -- wrapping the buffer and
                                                      -- overwriting events that
                                                      -- have not been consumed
                                                      -- yet.
  , rbCachedGatingSequence :: IORef SequenceNumber
  , rbAvailableBuffer      :: IOVector Int
  }

newRingBuffer :: RingBufferMode -> Int -> IO (RingBuffer e)
newRingBuffer mode capacity
  | capacity <= 0          =
      error "newRingBuffer: capacity must be greater than 0"
  | popCount capacity /= 1 =
      -- NOTE: The use of bitwise and (`.&.`) in `index` relies on this.
      error "newRingBuffer: capacity must be a power of 2"
  | otherwise              = do
      snr <- newIORef (-1)
      v   <- Vector.new capacity
      gs  <- newIORef []
      cgs <- newIORef (-1)
      ab  <- Vector.new (case mode of
                           SingleProducer -> 0
                           MultiProducer  -> capacity)
      Vector.set ab (-1)
      return (RingBuffer mode (fromIntegral capacity) snr v gs cgs ab)

ringBufferCapacity :: RingBuffer e -> Word64
ringBufferCapacity = rbCapacity

getCursor :: RingBuffer e -> IO SequenceNumber
getCursor rb = readIORef (rbSequenceNumber rb)

claim :: RingBuffer e -> SequenceNumber -> IO ()
claim rb = atomicWriteIORef (rbSequenceNumber rb)

setGatingSequences :: RingBuffer e -> [EventConsumer] -> IO ()
setGatingSequences rb eps =
  atomicWriteIORef (rbGatingSequences rb) (map ecSequenceNumber eps)

getCachedGatingSequence :: RingBuffer e -> IO SequenceNumber
getCachedGatingSequence rb = readIORef (rbCachedGatingSequence rb)

setCachedGatingSequence :: RingBuffer e -> SequenceNumber -> IO ()
setCachedGatingSequence rb = atomicWriteIORef (rbCachedGatingSequence rb)

setAvailable :: RingBuffer e -> SequenceNumber -> IO ()
setAvailable rb snr = Vector.write
  (rbAvailableBuffer rb)
  (index (rbCapacity rb) snr)
  (availabilityFlag (rbCapacity rb) snr)

getAvailable :: RingBuffer e -> Int -> IO Int
getAvailable rb ix = Vector.read (rbAvailableBuffer rb) ix

-- > quickCheck $ \(Positive i) j -> let capacity = 2^i in
--     j `mod` capacity == j Data.Bits..&. (capacity - 1)
index :: Word64 -> SequenceNumber -> Int
index capacity (SequenceNumber snr) = fromIntegral (snr .&. indexMask)
  where
    indexMask = capacity - 1

availabilityFlag :: Word64 -> SequenceNumber -> Int
availabilityFlag capacity (SequenceNumber snr) =
  fromIntegral (snr `shiftR` indexShift)
  where
    indexShift = logBase2 capacity

logBase2 :: Word64 -> Int
logBase2 w = finiteBitSize w - 1 - countLeadingZeros w

-- * Event producers

data EventProducer = EventProducer
  { epAsync :: Async ()
  }

newEventProducer :: RingBuffer e -> (s -> IO (e, s)) -> (s -> IO ()) -> s -> IO EventProducer
newEventProducer rb p backPressure s0 = do
  a <- async (go s0)
  return (EventProducer a)
  where
    go s = do
      mSnr <- tryNext rb
      case mSnr of
        Nothing -> do
          putStrLn "producer: consumer is too slow"
          backPressure s
          go s
        Just snr -> do
          (e, s') <- p s
          set rb snr e
          putStrLn ("wrote to srn: " ++ show (getSequenceNumber snr))
          publish rb snr
          go s'

-- | Claim the next event in sequence for publishing.
next :: RingBuffer e -> IO SequenceNumber
next rb = nextBatch rb 1

-- | Claim the next `n` events in sequence for publishing. This is for batch
-- event producing. Returns the highest claimed sequence number, so using it
-- requires a bit of extra work, e.g.:
--
-- @
--     let n = 10
--     hi <- nextBatch rb n
--     let lo = hi - (n - 1)
--     mapM_ f [lo..hi]
--     publishBatch rb lo hi
-- @
--
nextBatch :: RingBuffer e -> Int -> IO SequenceNumber
nextBatch rb n
  | n < 1 || fromIntegral n > ringBufferCapacity rb =
    error "nextBatch: n < 1 || n > ringBufferCapacity"
  | otherwise = do
    undefined
    -- XXX: ...

-- Try to return the next sequence number to write to. If `Nothing` is returned,
-- then the last consumer has not yet processed the event we are about to
-- overwrite (due to the ring buffer wrapping around) -- the callee of `tryNext`
-- should apply back-pressure upstream if this happens.
tryNext :: RingBuffer e -> IO (Maybe SequenceNumber)
tryNext rb = tryNextBatch rb 1

tryNextBatch :: RingBuffer e -> Int -> IO (Maybe SequenceNumber)
tryNextBatch rb n
  | n < 1     = error "tryNextBatch: n must be > 0"
  | otherwise = go
  where
    go = do
      current <- readForCAS (rbSequenceNumber rb)
      let current_ = peekTicket current
          next     = current_ + fromIntegral n
      hasCapacity <- hasAvailableCapacity' rb n current_
      if not hasCapacity
      then return Nothing
      else do
        -- NOTE: In the SingleProducer case we could just use writeIORef here,
        -- but not sure if it's worth the extra branch...
        (success, _current') <- casIORef (rbSequenceNumber rb) current next
        if success
        then return (Just next)
        else go

remainingCapacity :: RingBuffer e -> IO Word64
remainingCapacity rb = do
  consumed <- minimumSequence rb
  produced <- getCursor rb
  return (ringBufferCapacity rb - fromIntegral (produced - consumed))

hasAvailableCapacity :: RingBuffer e -> Int -> IO Bool
hasAvailableCapacity rb requiredCapacity = do
  cursorValue <- getCursor rb
  hasAvailableCapacity' rb requiredCapacity cursorValue

hasAvailableCapacity' :: RingBuffer e -> Int -> SequenceNumber -> IO Bool
hasAvailableCapacity' rb requiredCapacity cursorValue = do
  let wrapPoint = (cursorValue + fromIntegral requiredCapacity) -
                  fromIntegral (ringBufferCapacity rb)
  cachedGatingSequence <- getCachedGatingSequence rb
  if (wrapPoint > cachedGatingSequence || cachedGatingSequence > cursorValue)
  then do
    minSequence <- minimumSequence' (rbGatingSequences rb) cursorValue
    setCachedGatingSequence rb minSequence
    if (wrapPoint > minSequence)
    then return False
    else return True
  else return True

minimumSequence :: RingBuffer e -> IO SequenceNumber
minimumSequence rb = do
  cursorValue <- getCursor rb
  minimumSequence' (rbGatingSequences rb) cursorValue

minimumSequence' :: IORef [IORef SequenceNumber] -> SequenceNumber -> IO SequenceNumber
minimumSequence' gatingSequences cursorValue = do
  snrs <- mapM readIORef =<< readIORef gatingSequences
  return (minimum (cursorValue : snrs))

set :: RingBuffer e -> SequenceNumber -> e -> IO ()
set rb snr e = Vector.write (rbEvents rb) (index (rbCapacity rb) snr) e

-- TODO: Non-blocking multi-producer: https://youtu.be/VBnLW9mKMh4?t=1813
-- https://groups.google.com/g/lmax-disruptor/c/UhmRuz_CL6E/m/-hVt86bHvf8J
-- Multiple single-producers combined into one:
-- https://groups.google.com/g/lmax-disruptor/c/hvJVE-h2Xu0/m/mBW0j_3SrmIJ
publish :: RingBuffer e -> SequenceNumber -> IO ()
publish rb snr = case rbMode rb of
  SingleProducer -> claim rb snr
  MultiProducer  ->
    setAvailable rb snr
    -- XXX: Wake up consumers that are using a sleep wait strategy.

publishBatch :: RingBuffer e -> SequenceNumber -> SequenceNumber -> IO ()
publishBatch rb lo hi = case rbMode rb of
  SingleProducer -> claim rb hi
  MultiProducer  -> mapM_ (setAvailable rb) [lo..hi]
    -- XXX: Wake up consumers that are using a sleep wait strategy.

unsafeGet :: RingBuffer e -> SequenceNumber -> IO e
unsafeGet rb current = case rbMode rb of
  SingleProducer -> Vector.read (rbEvents rb) (index (rbCapacity rb) current)
  MultiProducer  -> go
  where
    availableValue :: Int
    availableValue = fromIntegral current `shiftR` indexShift

    ix :: Int
    ix = index (rbCapacity rb) current

    indexShift :: Int
    indexShift = logBase2 (rbCapacity rb)

    go = do
      v <- getAvailable rb ix
      if v /= availableValue
      then go -- XXX: a bit of sleep?
      else Vector.read (rbEvents rb) ix

get :: RingBuffer e -> SequenceNumber -> IO (Maybe e)
get rb snr = do
  written <- getCursor rb
  if snr <= written
  then Just <$> unsafeGet rb snr
  else return Nothing

-- * Event consumers

data EventConsumer = EventConsumer
  { ecSequenceNumber :: IORef SequenceNumber
  , ecAsync          :: Async ()
  }

-- NOTE: The `SequenceNumber` can be used for sharding, e.g. one handler handles
-- even and another handles odd numbers.
type EventHandler e = e -> SequenceNumber -> EndOfBatch -> IO ()
type EndOfBatch = Bool

data SequenceBarrier e
  = RingBufferBarrier (RingBuffer e)
  | EventConsumerBarrier EventConsumer

data WaitStrategy = Sleep Int

newEventConsumer :: EventHandler e -> RingBuffer e -> [SequenceBarrier e] -> WaitStrategy
                 -> IO EventConsumer
newEventConsumer handler rb barriers (Sleep n) = do
  putStrLn "starting consumer"
  snrRef <- newIORef (-1)
  a <- async (go snrRef) -- XXX: Pin to a specific CPU core with `asyncOn`?
  return (EventConsumer snrRef a)
  where
    go snrRef = do
      mySnr <- readIORef snrRef
      mbSnr <- waitFor mySnr rb barriers
      case mbSnr of
        Nothing -> do
          -- XXX: Other wait strategies could be implemented here, e.g. we could
          -- try to recurse immediately here, and if there's no work after a
          -- couple of tries go into a takeMTVar sleep waiting for a producer to
          -- wake us up.
          threadDelay n
          putStrLn ("nothing to do, mySrn = " ++ show (getSequenceNumber mySnr))
          -- XXX: Maybe we want to check if a shutdown variable has been set before looping?
          go snrRef
        Just bSnr -> do
          putStrLn ("something to do, mySrn = " ++ show (getSequenceNumber mySnr) ++
                    ", bSnr  = " ++ show (getSequenceNumber bSnr))
          -- XXX: what if handler throws exception? https://youtu.be/eTeWxZvlCZ8?t=2271
          mapM_ (\snr -> unsafeGet rb snr >>= \e ->
                    handler e snr (snr == bSnr)) [mySnr + 1..bSnr]
          writeIORef snrRef bSnr
          threadDelay 1000000
          go snrRef

waitFor :: SequenceNumber -> RingBuffer e -> [SequenceBarrier e] -> IO (Maybe SequenceNumber)
waitFor snr rb [] = waitFor snr rb [RingBufferBarrier rb]
waitFor snr rb bs = do
  let snrs = map getSequenceNumberRef bs
  minSnr <- minimum <$> mapM readIORef snrs
  putStrLn ("waitFor: snr = " ++ show (getSequenceNumber snr) ++
            ", minSrn = " ++ show (getSequenceNumber minSnr))
  if (snr == maxBound && minSnr /= maxBound) || snr < minSnr
  then return (Just minSnr)
  else return Nothing
  where
    getSequenceNumberRef :: SequenceBarrier e -> IORef SequenceNumber
    getSequenceNumberRef (RingBufferBarrier    rb) = rbSequenceNumber rb
    getSequenceNumberRef (EventConsumerBarrier ec) = ecSequenceNumber ec

main :: IO ()
main = do
  rb <- newRingBuffer SingleProducer 128
  let pipe = "/tmp/producer-pipe"
  safeCreateNamedPipe pipe
  h <- openFile pipe ReadWriteMode
  hSetBuffering h LineBuffering
  let backPressure = const (threadDelay 1000000)
  ep <- newEventProducer rb producer backPressure h
  link (epAsync ep)
  ec <- newEventConsumer handler rb [] (Sleep 1000000)
  setGatingSequences rb [ec]
  link (ecAsync ec)
  threadDelay 5000000
  cancel (epAsync ep)
  cancel (ecAsync ec)
  return ()
  where
    handler str snr eob = putStrLn (show (getSequenceNumber snr) ++ ": " ++ str ++
                                    if eob then ";" else "")
    producer h = do
      l <- hGetLine h
      return (l, h)

safeCreateNamedPipe :: FilePath -> IO ()
safeCreateNamedPipe fp =
  catchJust
    (\e -> if isAlreadyExistsErrorType (ioeGetErrorType e)
           then Just ()
           else Nothing)
    (createNamedPipe fp
      (namedPipeMode `unionFileModes`
       ownerReadMode `unionFileModes`
       ownerWriteMode))
    return
