{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module StuntDouble.Disruptor where

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
  deriving (Num, Eq, Ord, Real, Enum, Integral, Show)

-- * Ring-buffer

data RingBuffer e = RingBuffer
  { rbSequenceNumber  :: IORef SequenceNumber
  , rbEvents          :: IOVector e
  , rbGatingSequences :: IORef [IORef SequenceNumber] -- ^ References to the
                                                      -- last consumers'
                                                      -- sequence numbers, used
                                                      -- in order to avoid
                                                      -- wrapping the buffer and
                                                      -- overwriting events that
                                                      -- have not been consumed
                                                      -- yet.
  , rbCapacity        :: Word64
  }

newRingBuffer :: Int -> IO (RingBuffer e)
newRingBuffer capacity
  | capacity <= 0          =
      error "newRingBuffer: capacity must be greater than 0"
  | popCount capacity /= 1 =
      -- NOTE: The use of bitwise and (`.&.`) in `index` relies on this.
      error "newRingBuffer: capacity must be a power of 2"
  | otherwise              = do
      snr <- newIORef (-1)
      v   <- Vector.new capacity
      gs  <- newIORef []
      return (RingBuffer snr v gs (fromIntegral capacity))

setGatingSequences :: RingBuffer e -> [EventConsumer] -> IO ()
setGatingSequences rb eps =
  writeIORef (rbGatingSequences rb) (map ecSequenceNumber eps)

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
      mSnr <- next rb
      case mSnr of
        Nothing -> do
          putStrLn "producer: consumer is too slow"
          backPressure s
          go s
        Just snr -> do
          (e, s') <- p s
          write rb e snr
          -- putStrLn ("wrote to srn: " ++ show (getSequenceNumber snr))
          publish rb
          go s'

-- > quickCheck $ \(Positive i) j -> let capacity = 2^i in
--     j `mod` capacity == j Data.Bits..&. (capacity - 1)
index :: Word64 -> SequenceNumber -> Int
index capacity (SequenceNumber snr) = fromIntegral (snr .&. (capacity - 1))

-- The `next` sequence number to write to. If `Nothing` is returned, then the
-- last consumer has not yet processed the event we are about to overwrite (due
-- to the ring buffer wrapping around) -- the callee of `next` should apply
-- back-pressure upstream if this happens.
next :: RingBuffer e -> IO (Maybe SequenceNumber)
next rb = do
  nextSnr <- (+1) <$> readIORef (rbSequenceNumber rb)
  -- Check that the ring-buffer doesn't overwrite events that still haven't been
  -- processed by the last consumers.
  rc <- remainingCapacity rb nextSnr
  if rc <= 0
  then return Nothing
  else return (Just nextSnr) -- XXX: we could return a range `(nextSnr, nextSnr + rc)`
                             -- which could be used for batched writes.

remainingCapacity :: RingBuffer e -> SequenceNumber -> IO Word64
remainingCapacity rb produced = do
  snrs <- mapM readIORef =<< readIORef (rbGatingSequences rb)
  let consumed = minimum (if null snrs then [0] else snrs)
  return (rbCapacity rb - getSequenceNumber (produced - consumed))

write :: RingBuffer e -> e -> SequenceNumber -> IO ()
write rb e snr = Vector.write (rbEvents rb) (index (rbCapacity rb) snr) e

publish :: RingBuffer e -> IO ()
publish rb = atomicModifyIORef' (rbSequenceNumber rb) (\snr -> (snr + 1, ()))

get :: SequenceNumber -> RingBuffer e -> IO e
get snr rb = Vector.read (rbEvents rb) (index (rbCapacity rb) snr)
-- XXX: check if srn <= rbSequenceNumber?

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
          mapM_ (\snr -> get snr rb >>= \e -> handler e snr (snr == bSnr)) [mySnr + 1..bSnr]
          writeIORef snrRef bSnr
          threadDelay 1000000
          go snrRef

waitFor :: SequenceNumber -> RingBuffer e -> [SequenceBarrier e] -> IO (Maybe SequenceNumber)
waitFor snr rb [] = waitFor snr rb [RingBufferBarrier rb]
waitFor snr rb bs = do
  let snrs = concatMap getSequenceNumber bs
  minSrn <- minimum <$> mapM readIORef snrs
  if snr < minSrn
  then return (Just minSrn)
  else return Nothing
  where
    getSequenceNumber :: SequenceBarrier e -> [IORef SequenceNumber]
    getSequenceNumber (RingBufferBarrier    rb) = [rbSequenceNumber rb]
    getSequenceNumber (EventConsumerBarrier ec) = [ecSequenceNumber ec]

main :: IO ()
main = do
  rb <- newRingBuffer 128
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
  threadDelay 30000000
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
