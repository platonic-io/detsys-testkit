{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Disruptor where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (Async, withAsync, link)
import Control.Concurrent.STM
       (TMVar, atomically, isEmptyTMVar, newEmptyTMVarIO, tryPutTMVar)
import Control.Monad (foldM, void, when)
import Data.Atomics (casIORef, peekTicket, readForCAS)
import Data.Bits
       (countLeadingZeros, finiteBitSize, popCount, shiftR, (.&.))
import Data.IORef
       ( IORef
       , atomicModifyIORef'
       , atomicWriteIORef
       , newIORef
       , readIORef
       )
import Data.Int (Int64)
import Data.Vector.Mutable (IOVector)
import qualified Data.Vector.Mutable as Vector

------------------------------------------------------------------------

newtype SequenceNumber = SequenceNumber { getSequenceNumber :: Int64 }
  deriving (Num, Eq, Ord, Real, Enum, Integral, Show, Bounded)
-- ^ NOTE: `(maxBound :: Int64) == 9223372036854775807` so if we write 10M events
-- per second (`10_000_000*60*60*24*365 == 315360000000000) then it would take
-- us `9223372036854775807 / 315360000000000 == 29247.1208677536` years before
-- we overflow.

-- * Ring-buffer

data RingBufferMode
  = SingleProducer

  -- | The lock-free multi-producer implementation is presented in the following
  -- talk:
  --
  --   https://youtu.be/VBnLW9mKMh4?t=1813
  --
  -- and also discussed in the following thread:
  --
  --   https://groups.google.com/g/lmax-disruptor/c/UhmRuz_CL6E/m/-hVt86bHvf8J
  --
  -- Note that one can also achieve a similar result by using multiple
  -- single-producers and combine them into one as outlined in this thread:
  --
  -- https://groups.google.com/g/lmax-disruptor/c/hvJVE-h2Xu0/m/mBW0j_3SrmIJ
  | MultiProducer

data RingBuffer e = RingBuffer
  { rbMode            :: RingBufferMode
  , rbCapacity        :: Int64
  , rbSequenceNumber  :: IORef SequenceNumber
  , rbEvents          :: IOVector e
  -- | References to the last consumers' sequence numbers, used in order to
  -- avoid wrapping the buffer and overwriting events that have not been
  -- consumed yet.
  , rbGatingSequences      :: IORef [IORef SequenceNumber]
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

ringBufferCapacity :: RingBuffer e -> Int64
ringBufferCapacity = rbCapacity

getCursor :: RingBuffer e -> IO SequenceNumber
getCursor rb = readIORef (rbSequenceNumber rb)

claim :: RingBuffer e -> SequenceNumber -> IO ()
claim rb = atomicWriteIORef (rbSequenceNumber rb)

data Exists f = forall x. Exists (f x)

setGatingSequences :: RingBuffer e -> [Exists EventConsumer] -> IO ()
setGatingSequences rb eps =
  atomicWriteIORef (rbGatingSequences rb) (map go eps)
  where
    go (Exists ec) = ecSequenceNumber ec

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
index :: Int64 -> SequenceNumber -> Int
index capacity (SequenceNumber snr) = fromIntegral (snr .&. indexMask)
  where
    indexMask = capacity - 1

availabilityFlag :: Int64 -> SequenceNumber -> Int
availabilityFlag capacity (SequenceNumber snr) =
  fromIntegral (snr `shiftR` indexShift)
  where
    indexShift = logBase2 capacity

logBase2 :: Int64 -> Int
logBase2 w = finiteBitSize w - 1 - countLeadingZeros w

-- * Event producers

data EventProducer s = EventProducer
  { epWorker       :: s -> IO s
  , epInitialState :: s
  , epShutdown     :: Shutdown
  }

newEventProducer :: RingBuffer e -> (s -> IO (e, s)) -> (s -> IO ()) -> s
                 -> IO (EventProducer s)
newEventProducer rb p backPressure s0 = do
  shutdownVar <- newShutdownVar
  let go s = do
        mSnr <- tryNext rb
        case mSnr of
          Nothing -> do
            putStrLn "producer: consumer is too slow"
            backPressure s
            halt <- isItTimeToShutdown shutdownVar
            if halt
            then return s
            else go s
          Just snr -> do
            (e, s') <- p s
            set rb snr e
            -- XXX: removing the following line causes a loop.
            threadDelay 10
            publish rb snr
            putStrLn ("wrote to srn: " ++ show (getSequenceNumber snr))
            halt <- isItTimeToShutdown shutdownVar
            if halt
            then return s'
            else go s'

  return (EventProducer go s0 shutdownVar)

withEventProducer :: EventProducer s -> (Async s -> IO a) -> IO a
withEventProducer ep k = withAsync (epWorker ep (epInitialState ep)) $ \a -> do
  link a
  k a

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
      error "nextBatch: n must be >= 1 and =< ringBufferCapacity"
  -- NOTE: This is the multiple-producer case, we might want to add a simpler
  -- single-producer case also.
  | otherwise = do
      (current, nextSequence) <- atomicModifyIORef' (rbSequenceNumber rb) $ \current ->
                                   let
                                     nextSequence = current + fromIntegral n
                                   in
                                     (nextSequence, (current, nextSequence))

      let wrapPoint :: SequenceNumber
          wrapPoint = nextSequence - fromIntegral (ringBufferCapacity rb)

      cachedGatingSequence <- getCachedGatingSequence rb

      when (wrapPoint > cachedGatingSequence || cachedGatingSequence > current) $
        waitForConsumers wrapPoint

      return nextSequence
  where
    waitForConsumers :: SequenceNumber -> IO ()
    waitForConsumers wrapPoint = go
      where
        go :: IO ()
        go = do
          gatingSequence <- minimumSequence rb
          if wrapPoint > gatingSequence
          then do
            threadDelay 1
            go
          else setCachedGatingSequence rb gatingSequence

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
      b <- hasCapacity' rb n current_
      if not b
      then return Nothing
      else do
        -- NOTE: In the SingleProducer case we could just use writeIORef here,
        -- but not sure if it's worth the extra branch...
        (success, _current') <- casIORef (rbSequenceNumber rb) current next
        if success
        then return (Just next)
        else go

remainingCapacity :: RingBuffer e -> IO Int64
remainingCapacity rb = do
  consumed <- minimumSequence rb
  produced <- getCursor rb
  return (ringBufferCapacity rb - fromIntegral (produced - consumed))

hasCapacity :: RingBuffer e -> Int -> IO Bool
hasCapacity rb requiredCapacity = do
  cursorValue <- getCursor rb
  hasCapacity' rb requiredCapacity cursorValue

hasCapacity' :: RingBuffer e -> Int -> SequenceNumber -> IO Bool
hasCapacity' rb requiredCapacity cursorValue = do
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

isAvailable :: RingBuffer e -> SequenceNumber -> IO Bool
isAvailable rb snr =
  (==) <$> Vector.read (rbAvailableBuffer rb) (index capacity snr)
       <*> pure (availabilityFlag capacity snr)
  where
    capacity = rbCapacity rb

highestPublished :: RingBuffer e -> SequenceNumber -> SequenceNumber
                 -> IO SequenceNumber
highestPublished rb lowerBound availableSequence = go lowerBound
  where
    go sequence
      | sequence > availableSequence = return availableSequence
      | otherwise                    = do
          available <- isAvailable rb sequence
          if not (available)
          then return (sequence - 1)
          else go (sequence + 1)

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

data EventConsumer s = EventConsumer
  { ecSequenceNumber :: IORef SequenceNumber
  , ecWorker         :: s -> IO s
  , ecInitialState   :: s
  , ecShutdown       :: Shutdown
  }

-- NOTE: The `SequenceNumber` can be used for sharding, e.g. one handler handles
-- even and another handles odd numbers.
type EventHandler s e = s -> e -> SequenceNumber -> EndOfBatch -> IO s
type EndOfBatch = Bool

data SequenceBarrier e
  = RingBufferBarrier (RingBuffer e)
  | forall s. EventConsumerBarrier (EventConsumer s)

data WaitStrategy = Sleep Int

withEventConsumer :: EventConsumer s -> (Async s -> IO a) -> IO a
withEventConsumer ec k = withAsync (ecWorker ec (ecInitialState ec)) $ \a -> do
  link a
  k a
  -- ^ XXX: Pin to a specific CPU core with `withAsyncOn`?

newtype Shutdown = Shutdown (TMVar ())

newShutdownVar :: IO Shutdown
newShutdownVar = Shutdown <$> newEmptyTMVarIO

tellToShutdown :: Shutdown -> IO ()
tellToShutdown (Shutdown tmvar) = void (atomically (tryPutTMVar tmvar ()))

shutdownProducer :: EventProducer s -> IO ()
shutdownProducer = tellToShutdown . epShutdown

shutdownConsumer :: EventConsumer s -> IO ()
shutdownConsumer = tellToShutdown . ecShutdown

isItTimeToShutdown :: Shutdown -> IO Bool
isItTimeToShutdown (Shutdown tmvar) = not <$> atomically (isEmptyTMVar tmvar)

newEventConsumer :: RingBuffer e -> EventHandler s e -> s -> [SequenceBarrier e]
                 -> WaitStrategy -> IO (EventConsumer s)
newEventConsumer rb handler s0 barriers (Sleep n) = do
  snrRef <- newIORef (-1)
  shutdownVar <- newShutdownVar

  let go s = do
        mySnr <- readIORef snrRef
        mbSnr <- waitFor mySnr rb barriers
        case mbSnr of
          Nothing -> do
            -- XXX: Other wait strategies could be implemented here, e.g. we could
            -- try to recurse immediately here, and if there's no work after a
            -- couple of tries go into a takeMTVar sleep waiting for a producer to
            -- wake us up.
            threadDelay n
            halt <- isItTimeToShutdown shutdownVar
            if halt
            then return s
            else go s
          Just bSnr -> do
            -- XXX: what if handler throws exception? https://youtu.be/eTeWxZvlCZ8?t=2271
            s' <- foldM (\ih snr -> unsafeGet rb snr >>= \e ->
                            handler ih e snr (snr == bSnr)) s [mySnr + 1..bSnr]
            atomicWriteIORef snrRef bSnr
            halt <- isItTimeToShutdown shutdownVar
            if halt
            then return s'
            else go s'

  return (EventConsumer snrRef go s0 shutdownVar)

waitFor :: SequenceNumber -> RingBuffer e -> [SequenceBarrier e] -> IO (Maybe SequenceNumber)
waitFor snr rb [] = waitFor snr rb [RingBufferBarrier rb]
waitFor snr rb bs = do
  let snrs = map getSequenceNumberRef bs
  minSnr <- minimum <$> mapM readIORef snrs
  putStrLn $ "waitFor: snr = " ++ show (getSequenceNumber snr) ++ ", minSrn = " ++ show (getSequenceNumber minSnr)
  if snr < minSnr
  then return (Just minSnr)
  else return Nothing
  where
    getSequenceNumberRef :: SequenceBarrier e -> IORef SequenceNumber
    getSequenceNumberRef (RingBufferBarrier    rb) = rbSequenceNumber rb
    getSequenceNumberRef (EventConsumerBarrier ec) = ecSequenceNumber ec
