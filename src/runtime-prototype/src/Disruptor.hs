{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Disruptor where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (Async, link, withAsync, withAsyncOn)
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
       , writeIORef
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

data ProducerType
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
  {
  -- | Keeps track of if the ring buffer was created for single or multiple
  -- producer(s).
    rbProducerType :: !ProducerType
  -- | The capacity, or maximum amount of values, of the ring buffer.
  , rbCapacity :: {-# UNPACK #-} !Int64
  -- | The cursor pointing to the head of the ring buffer.
  , rbSequenceNumber :: {-# UNPACK #-} !(IORef SequenceNumber)
  -- | The values of the ring buffer.
  , rbEvents :: {-# UNPACK #-} !(IOVector e)
  -- | References to the last consumers' sequence numbers, used in order to
  -- avoid wrapping the buffer and overwriting events that have not been
  -- consumed yet.
  , rbGatingSequences :: {-# UNPACK #-} !(IORef [IORef SequenceNumber])
  -- | Cached value of computing the last consumers' sequence numbers using the
  -- above references.
  , rbCachedGatingSequence :: {-# UNPACK #-} !(IORef SequenceNumber)
  -- | Used to keep track of what has been published in the multi-producer case.
  , rbAvailableBuffer :: {-# UNPACK #-} !(IOVector Int)
  }

newRingBuffer :: ProducerType -> Int -> IO (RingBuffer e)
newRingBuffer producerType capacity
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
      ab  <- Vector.new (case producerType of
                           SingleProducer -> 0
                           MultiProducer  -> capacity)
      Vector.set ab (-1)
      return (RingBuffer producerType (fromIntegral capacity) snr v gs cgs ab)

ringBufferCapacity :: RingBuffer e -> Int64
ringBufferCapacity = rbCapacity

getCursor :: RingBuffer e -> IO SequenceNumber
getCursor rb = readIORef (rbSequenceNumber rb)

{-# INLINE claim #-}
claim :: RingBuffer e -> SequenceNumber -> IO ()
claim rb = writeIORef (rbSequenceNumber rb)

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

{-# INLINE setAvailable #-}
setAvailable :: RingBuffer e -> SequenceNumber -> IO ()
setAvailable rb snr = Vector.write
  (rbAvailableBuffer rb)
  (index (rbCapacity rb) snr)
  (availabilityFlag (rbCapacity rb) snr)

{-# INLINE getAvailable #-}
getAvailable :: RingBuffer e -> Int -> IO Int
getAvailable rb ix = Vector.read (rbAvailableBuffer rb) ix

-- > quickCheck $ \(Positive i) j -> let capacity = 2^i in
--     j `mod` capacity == j Data.Bits..&. (capacity - 1)
{-# INLINE index #-}
index :: Int64 -> SequenceNumber -> Int
index capacity (SequenceNumber snr) = fromIntegral (snr .&. indexMask)
  where
    indexMask = capacity - 1

{-# INLINE availabilityFlag #-}
availabilityFlag :: Int64 -> SequenceNumber -> Int
availabilityFlag capacity (SequenceNumber snr) =
  fromIntegral (snr `shiftR` indexShift)
  where
    indexShift = logBase2 capacity

-- Taken from:
-- https://hackage.haskell.org/package/base-4.15.0.0/docs/Data-Bits.html#v:countLeadingZeros
{-# INLINE logBase2 #-}
logBase2 :: Int64 -> Int
logBase2 i = finiteBitSize i - 1 - countLeadingZeros i

-- * Event producers

data EventProducer s = EventProducer
  { epWorker       :: s -> IO s
  , epInitialState :: s
  , epShutdown     :: Shutdown
  }

newEventProducer' :: RingBuffer e -> (s -> IO (e, s)) -> (s -> IO ()) -> s
                  -> IO (EventProducer s)
newEventProducer' rb p backPressure s0 = do
  shutdownVar <- newShutdownVar
  let go s = do
        let n = 1024
        hi <- nextBatch rb n
        let lo = hi - fromIntegral (n - 1)
        s' <- foldM (\ih snr -> p ih >>= \(e, s') -> set rb snr e >> return s') s [lo..hi]
        publishBatch rb lo hi
        halt <- isItTimeToShutdown shutdownVar
        if halt
        then return s'
        else go s'

  return (EventProducer go s0 shutdownVar)

newEventProducer :: RingBuffer e -> (s -> IO (e, s)) -> (s -> IO ()) -> s
                 -> IO (EventProducer s)
newEventProducer rb p backPressure s0 = do
  shutdownVar <- newShutdownVar
  let go s = {-# SCC go #-} do
        mSnr <- tryNext rb
        case mSnr of
          Nothing -> do
            {-# SCC backPresure #-} backPressure s
            halt <- isItTimeToShutdown shutdownVar
            if halt
            then return s
            else go s
          Just snr -> do
            (e, s') <- {-# SCC p #-} p s
            set rb snr e
            publish rb snr
            halt <- isItTimeToShutdown shutdownVar
            if halt
            then return s'
            else go s' -- SPIN

  return (EventProducer go s0 shutdownVar)

withEventProducer :: EventProducer s -> (Async s -> IO a) -> IO a
withEventProducer ep k = withAsync (epWorker ep (epInitialState ep)) $ \a -> do
  link a
  k a

withEventProducerOn :: Int -> EventProducer s -> (Async s -> IO a) -> IO a
withEventProducerOn capability ep k =
  withAsyncOn capability (epWorker ep (epInitialState ep)) $ \a -> do
    link a
    k a

-- | Claim the next event in sequence for publishing.
{-# INLINE next #-}
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
      error ("nextBatch: n (= " ++ show n ++ ") must be >= 1 and =< ringBufferCapacity")
  | otherwise = case rbProducerType rb of
      SingleProducer -> do
        current <- getCursor rb
        let nextSequence :: SequenceNumber
            nextSequence = current + fromIntegral n

            wrapPoint :: SequenceNumber
            wrapPoint = nextSequence - fromIntegral (ringBufferCapacity rb)

        claim rb nextSequence
        cachedGatingSequence <- getCachedGatingSequence rb

        when (wrapPoint > cachedGatingSequence || cachedGatingSequence > current) $
          waitForConsumers wrapPoint

        return nextSequence

      MultiProducer  -> do
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
          then go
          else setCachedGatingSequence rb gatingSequence

-- Try to return the next sequence number to write to. If `Nothing` is returned,
-- then the last consumer has not yet processed the event we are about to
-- overwrite (due to the ring buffer wrapping around) -- the callee of `tryNext`
-- should apply back-pressure upstream if this happens.
{-# INLINE tryNext #-}
tryNext :: RingBuffer e -> IO (Maybe SequenceNumber)
tryNext rb = tryNextBatch rb 1

tryNextBatch :: RingBuffer e -> Int -> IO (Maybe SequenceNumber)
tryNextBatch rb n
  | n < 1     = error "tryNextBatch: n must be > 0"
  | otherwise = case rbProducerType rb of
      SingleProducer -> sp
      MultiProducer  -> mp
  where
    sp :: IO (Maybe SequenceNumber)
    sp = do
      current <- readIORef (rbSequenceNumber rb)
      let next = current + fromIntegral n
      b <- hasCapacity' rb n current
      if not b
      then return Nothing
      else return (Just next)

    mp :: IO (Maybe SequenceNumber)
    mp = do
      current <- readForCAS (rbSequenceNumber rb)
      let current_ = peekTicket current
          next     = current_ + fromIntegral n
      b <- hasCapacity' rb n current_
      if not b
      then return Nothing
      else do
        (success, _current') <- casIORef (rbSequenceNumber rb) current next
        if success
        then return (Just next)
        else mp

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
publish rb snr = case rbProducerType rb of
  SingleProducer -> claim rb snr
  MultiProducer  ->
    setAvailable rb snr
    -- XXX: Wake up consumers that are using a sleep wait strategy.

publishBatch :: RingBuffer e -> SequenceNumber -> SequenceNumber -> IO ()
publishBatch rb lo hi = case rbProducerType rb of
  SingleProducer -> claim rb hi
  MultiProducer  -> mapM_ (setAvailable rb) [lo..hi]
    -- XXX: Wake up consumers that are using a sleep wait strategy.

unsafeGetSP :: RingBuffer e -> SequenceNumber -> IO e
unsafeGetSP rb current = Vector.read (rbEvents rb) (index (rbCapacity rb) current)
{-# INLINE unsafeGetSP #-}

unsafeGet :: RingBuffer e -> SequenceNumber -> IO e
unsafeGet rb current = case rbProducerType rb of
  SingleProducer -> Vector.read (rbEvents rb) (index (rbCapacity rb) current)
  MultiProducer  -> go
  where
    availableValue :: Int
    availableValue = availabilityFlag (rbCapacity rb) current

    ix :: Int
    ix = index (rbCapacity rb) current

    go = do
      v <- getAvailable rb ix
      if v /= availableValue
      then go
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

withEventConsumerOn :: Int -> EventConsumer s -> (Async s -> IO a) -> IO a
withEventConsumerOn capability ec k =
  withAsyncOn capability (ecWorker ec (ecInitialState ec)) $ \a -> do
    link a
    k a

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

  let go s = {-# SCC go #-} do
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
            s' <- {-# SCC go' #-} go' (mySnr + 1) bSnr s
            writeIORef snrRef bSnr
            halt <- isItTimeToShutdown shutdownVar
            if halt
            then return s'
            else go s' -- SPIN
            where
              go' lo hi s | lo >  hi = return s
                          | lo <= hi = do
                e <- unsafeGetSP rb lo
                s' <- {-# SCC handler #-} handler s e lo (lo == hi)
                go' (lo + 1) hi s'

  return (EventConsumer snrRef go s0 shutdownVar)

waitFor :: SequenceNumber -> RingBuffer e -> [SequenceBarrier e] -> IO (Maybe SequenceNumber)
waitFor snr rb [] = waitFor snr rb [RingBufferBarrier rb]
waitFor snr rb bs = do
  minSnr <- minimum <$> mapM readIORef (map getSequenceNumberRef bs)
  if snr < minSnr
  then return (Just minSnr)
  else return Nothing
  where
    getSequenceNumberRef :: SequenceBarrier e -> IORef SequenceNumber
    getSequenceNumberRef (RingBufferBarrier    rb) = rbSequenceNumber rb
    getSequenceNumberRef (EventConsumerBarrier ec) = ecSequenceNumber ec
