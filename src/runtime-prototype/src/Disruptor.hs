{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module StuntDouble.Disruptor where

import Control.Concurrent
import Control.Concurrent.Async
import Control.Exception
import Control.Monad
import Data.IORef
import Data.Int
import Data.Vector.Mutable (IOVector)
import qualified Data.Vector.Mutable as Vector
import System.IO
import System.IO.Error
import System.Posix.Files

------------------------------------------------------------------------

newtype SequenceNumber = SequenceNumber { getSequenceNumber :: Int64 }
  deriving (Num, Eq, Ord, Real, Enum, Integral)

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
  , rbCapacity        :: Int
  }

newRingBuffer :: Int -> IO (RingBuffer e)
newRingBuffer capacity = do
  snr <- newIORef (-1)
  v   <- Vector.new capacity
  gs  <- newIORef []
  return (RingBuffer snr v gs capacity)

setGatingSequences :: RingBuffer e -> [EventConsumer] -> IO ()
setGatingSequences rb eps =
  writeIORef (rbGatingSequences rb) (map ecSequenceNumber eps)

-- * Event producers

data EventProducer = EventProducer
  { epAsync :: Async ()
  }

newEventProducer :: RingBuffer e -> (s -> IO (e, s)) -> s -> IO EventProducer
newEventProducer rb p s0 = do
  a <- async (go s0)
  return (EventProducer a)
  where
    go s = do
      mSnr <- next rb
      case mSnr of
        Nothing -> do
          putStrLn "producer: consumer is too slow"
          threadDelay 1000000
          go s
        Just snr -> do
          (e, s') <- p s
          write rb e snr
          -- putStrLn ("wrote to srn: " ++ show (getSequenceNumber snr))
          publish rb
          go s'

index :: Int -> SequenceNumber -> Int
index capacity snr = fromIntegral (snr `mod` fromInteger (toInteger capacity))

-- The `next` sequence number to write to. If `Nothing` is returned, then the
-- last consumer has not yet processed the event we are about to overwrite (due
-- to the ring buffer wrapping around) -- the callee of `next` should apply
-- back-pressure upstream if this happens.
next :: RingBuffer e -> IO (Maybe SequenceNumber)
next rb = do
  nextSnr <- succ <$> readIORef (rbSequenceNumber rb)
  -- Check that the ring-buffer doesn't overwrite events that still haven't been
  -- processed by the last consumers.
  snrs <- mapM readIORef =<< readIORef (rbGatingSequences rb)
  if any (\snr -> overflow (rbCapacity rb) nextSnr snr) snrs
  then do
    putStrLn ("next: waiting for " ++ show (map (index (rbCapacity rb)) snrs) ++
              ", index rb nextSnr = " ++ show (index (rbCapacity rb) nextSnr))
    return Nothing
  else return (Just nextSnr)

-- XXX: what's the right way to calculate this?
overflow :: Int -> SequenceNumber -> SequenceNumber -> Bool
overflow capacity nextSnr lastConsumerSnr
  = index capacity nextSnr <= index capacity lastConsumerSnr

test_overflow1 = overflow 3 1 0 == False
test_overflow2 = overflow 3 3 0 == True
test_overflow3 = overflow 3 3 1 == False
test_overflow4 = overflow 3 4 1 == True
test_overflow5 = overflow 3 3 3 == False
test_overflow6 = overflow 3 6 3 == True

write :: RingBuffer e -> e -> SequenceNumber -> IO ()
write rb e snr = Vector.write (rbEvents rb) (index (rbCapacity rb) snr) e

publish :: RingBuffer e -> IO ()
publish rb = atomicModifyIORef' (rbSequenceNumber rb) (\snr -> (succ snr, ()))

get :: SequenceNumber -> RingBuffer e -> IO e
get snr rb = Vector.read (rbEvents rb) (index (rbCapacity rb) snr)

-- * Event consumers

data EventConsumer = EventConsumer
  { ecSequenceNumber :: IORef SequenceNumber
  , ecAsync          :: Async ()
  }

-- NOTE: The `SequenceNumber` can be used for sharding, e.g. one handler handles
-- even and another handles odd numbers.
type EventHandler e = e -> SequenceNumber -> EndOfBatch -> IO ()
type EndOfBatch = Bool

data Barrier e
  = RingBufferBarrier (RingBuffer e)
  | SequenceBarrierBarrier SequenceBarrier
  | EventConsumerBarrier EventConsumer

newEventConsumer :: EventHandler e -> RingBuffer e -> [Barrier e]
                  -> IO EventConsumer
newEventConsumer handler rb barriers = do
  putStrLn "starting processor"
  snrRef <- newIORef (-1)
  a <- async (go snrRef) -- XXX: Pin to a specific CPU core with `asyncOn`?
  return (EventConsumer snrRef a)
  where
    go snrRef = do
      mySnr <- readIORef snrRef
      mbSnr <- waitFor mySnr rb barriers
      case mbSnr of
        Nothing -> do
          -- XXX: We could try to recurse immediately here, and if there's no work
          -- after a couple of tries go into a takeMTVar sleep waiting for a
          -- producer to wake us up.
          threadDelay 1000000
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

waitFor :: SequenceNumber -> RingBuffer e -> [Barrier e] -> IO (Maybe SequenceNumber)
waitFor snr rb [] = waitFor snr rb [RingBufferBarrier rb]
waitFor snr rb bs = do
  let snrs = concatMap getSequenceNumber bs
  minSrn <- minimum <$> mapM readIORef snrs
  if snr < minSrn
  then return (Just minSrn)
  else return Nothing
  where
    getSequenceNumber :: Barrier e -> [IORef SequenceNumber]
    getSequenceNumber (RingBufferBarrier      rb) = [rbSequenceNumber rb]
    getSequenceNumber (SequenceBarrierBarrier sb) = sbSequenceNumbers sb
    getSequenceNumber (EventConsumerBarrier   ec) = [ecSequenceNumber ec]

data SequenceBarrier = SequenceBarrier
  { sbSequenceNumbers :: [IORef SequenceNumber] }

newSequenceBarrier :: [Either (RingBuffer e) EventConsumer] -> SequenceBarrier
newSequenceBarrier = SequenceBarrier . map (either rbSequenceNumber ecSequenceNumber)

main :: IO ()
main = do
  rb <- newRingBuffer 128
  let pipe = "/tmp/producer-pipe"
  safeCreateNamedPipe pipe
  h <- openFile pipe ReadWriteMode
  hSetBuffering h LineBuffering
  ep <- newEventProducer rb producer h
  link (epAsync ep)
  ec <- newEventConsumer handler rb []
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
