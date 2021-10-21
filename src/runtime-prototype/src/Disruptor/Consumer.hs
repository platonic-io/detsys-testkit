{-# LANGUAGE ExistentialQuantification #-} -- XXX

module Disruptor.Consumer where

import Control.Concurrent.Async
import Control.Concurrent
import Control.Concurrent.STM -- XXX
import Data.IORef

import Disruptor.SequenceNumber
import Disruptor.RingBuffer.SingleProducer

------------------------------------------------------------------------

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
                e <- unsafeGet rb lo
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
    getSequenceNumberRef (RingBufferBarrier    rb) = rbCursor rb
    getSequenceNumberRef (EventConsumerBarrier ec) = ecSequenceNumber ec

------------------------------------------------------------------------

newtype Shutdown = Shutdown (TMVar ())

newShutdownVar :: IO Shutdown
newShutdownVar = Shutdown <$> newEmptyTMVarIO

tellToShutdown :: Shutdown -> IO ()
tellToShutdown (Shutdown tmvar) = () <$ atomically (tryPutTMVar tmvar ())

shutdownConsumer :: EventConsumer s -> IO ()
shutdownConsumer = tellToShutdown . ecShutdown

isItTimeToShutdown :: Shutdown -> IO Bool
isItTimeToShutdown (Shutdown tmvar) = not <$> atomically (isEmptyTMVar tmvar)
