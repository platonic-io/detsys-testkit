{-# LANGUAGE ExistentialQuantification #-} -- XXX

module Disruptor.ConsumerUnboxed where

import Control.Concurrent.Async
import Control.Concurrent
import Control.Concurrent.STM -- XXX
import Data.IORef.Unboxed
import Data.Vector.Unboxed (Unbox)

import Disruptor.SequenceNumber
import Disruptor.RingBuffer.SingleProducerUnboxed

------------------------------------------------------------------------

data EventConsumer s = EventConsumer
  { ecSequenceNumber :: {-# UNPACK #-} !(IORefU SequenceNumber)
  , ecWorker         :: s -> IO s
  , ecInitialState   :: s
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

newEventConsumer :: Unbox e => RingBuffer e -> EventHandler s e -> s -> [SequenceBarrier e]
                 -> WaitStrategy -> IO (EventConsumer s)
newEventConsumer rb handler s0 barriers (Sleep n) = do
  snrRef <- newIORefU (-1)

  let go s = {-# SCC go #-} do
        mySnr <- readIORefU snrRef
        mbSnr <- waitFor mySnr rb barriers
        case mbSnr of
          Nothing -> do
            -- XXX: Other wait strategies could be implemented here, e.g. we could
            -- try to recurse immediately here, and if there's no work after a
            -- couple of tries go into a takeMTVar sleep waiting for a producer to
            -- wake us up.
            threadDelay n
            go s -- SPIN
          Just bSnr -> do
            -- XXX: what if handler throws exception? https://youtu.be/eTeWxZvlCZ8?t=2271
            s' <- {-# SCC go' #-} go' (mySnr + 1) bSnr s
            writeIORefU snrRef bSnr
            go s'
            where
              go' lo hi s | lo >  hi = return s
                          | lo <= hi = do
                e <- unsafeGet rb lo
                s' <- {-# SCC handler #-} handler s e lo (lo == hi)
                go' (lo + 1) hi s'

  return (EventConsumer snrRef go s0)

waitFor :: SequenceNumber -> RingBuffer e -> [SequenceBarrier e] -> IO (Maybe SequenceNumber)
waitFor snr rb [] = do
  minSnr <- readIORefU (rbCursor rb)
  if snr < minSnr
  then return (Just minSnr)
  else return Nothing
waitFor snr rb bs = do
  minSnr <- minimum <$> mapM readIORefU (map getSequenceNumberRef bs)
  if snr < minSnr
  then return (Just minSnr)
  else return Nothing
  where
    getSequenceNumberRef :: SequenceBarrier e -> IORefU SequenceNumber
    getSequenceNumberRef (RingBufferBarrier    rb) = rbCursor rb
    getSequenceNumberRef (EventConsumerBarrier ec) = ecSequenceNumber ec
{-# INLINE waitFor #-}
