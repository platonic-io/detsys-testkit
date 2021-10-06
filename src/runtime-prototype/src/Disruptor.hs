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

setGatingSequences :: RingBuffer e -> [EventProcessor] -> IO ()
setGatingSequences rb eps =
  writeIORef (rbGatingSequences rb) (map epSequenceNumber eps)

type EndOfBatch = Bool

data EventProcessor = EventProcessor
  { epSequenceNumber :: IORef SequenceNumber
  , epAsync          :: Async ()
  }

-- NOTE: The `SequenceNumber` can be used for sharding, e.g. one handler handles
-- even and another handles odd numbers.
type EventHandler e = e -> SequenceNumber -> EndOfBatch -> IO ()

data Barrier e
  = RingBufferBarrier (RingBuffer e)
  | SequenceBarrierBarrier SequenceBarrier
  | EventProducerBarrier EventProcessor

newEventProcessor :: EventHandler e -> RingBuffer e -> [Barrier e]
                  -> IO EventProcessor
newEventProcessor handler rb barriers = do
  putStrLn "starting processor"
  snrRef <- newIORef (-1)
  a <- async (go snrRef) -- XXX: Pin to a specific CPU core with `asyncOn`?
  return (EventProcessor snrRef a)
  where
    go snrRef = do
      mySnr <- readIORef snrRef
      rbSnr <- waitFor mySnr rb barriers
      if mySnr < rbSnr
      then do
        putStrLn ("something to do, mySrn = " ++ show (getSequenceNumber mySnr) ++
                  ", rbSnr  = " ++ show (getSequenceNumber rbSnr))
        mapM_ (\snr -> get snr rb >>= \e -> handler e snr (snr == rbSnr)) [mySnr + 1..rbSnr]
        writeIORef snrRef rbSnr
        threadDelay 1000000
        go snrRef
      else do
        -- XXX: We could try to recurse immediately here, and if there's no work
        -- after a couple of tries go into a takeMTVar sleep waiting for a
        -- producer to wake us up.
        threadDelay 1000000
        putStrLn ("nothing to do, mySrn = " ++ show (getSequenceNumber mySnr) ++
                  ", rbSnr  = " ++ show (getSequenceNumber rbSnr))
        -- XXX: Maybe we want to check if a shutdown variable has been set before looping?
        go snrRef

waitFor :: SequenceNumber -> RingBuffer e -> [Barrier e] -> IO SequenceNumber
waitFor snr rb [] = readIORef (rbSequenceNumber rb)
waitFor snr rb bs = do
  let snrs = concatMap getSequenceNumber bs
  minimum <$> mapM readIORef snrs
  where
    getSequenceNumber :: Barrier e -> [IORef SequenceNumber]
    getSequenceNumber (RingBufferBarrier      rb) = [rbSequenceNumber rb]
    getSequenceNumber (SequenceBarrierBarrier sb) = sbSequenceNumbers sb
    getSequenceNumber (EventProducerBarrier   ep) = [epSequenceNumber ep]

data SequenceBarrier = SequenceBarrier
  { sbSequenceNumbers :: [IORef SequenceNumber] }

newSequenceBarrier :: [Either (RingBuffer e) EventProcessor] -> SequenceBarrier
newSequenceBarrier = SequenceBarrier . map (either rbSequenceNumber epSequenceNumber)

main :: IO ()
main = do
  rb <- newRingBuffer 128
  let pipe = "/tmp/producer-pipe"
  safeCreateNamedPipe pipe
  h <- openFile pipe ReadWriteMode
  hSetBuffering h LineBuffering
  ap <- async (producer h rb)
  link ap
  ep <- newEventProcessor handler rb []
  setGatingSequences rb [ep]
  link (epAsync ep)
  threadDelay 30000000
  cancel ap
  cancel (epAsync ep)
  return ()
  where
    handler str snr eob = putStrLn (show (getSequenceNumber snr) ++ ": " ++ str ++
                                    if eob then ";" else "")
    producer h rb = forever $ do
      mSnr <- next rb
      case mSnr of
        Nothing -> do
          putStrLn "producer: consumer is too slow"
          threadDelay 1000000
        Just snr -> do
          l <- hGetLine h
          -- putStrLn ("got line: " ++ l)
          write rb l snr
          -- putStrLn ("wrote to srn: " ++ show (getSequenceNumber snr))
          publish rb

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
