module Disruptor.Producer where

import Control.Concurrent.Async
import Control.Concurrent.STM -- XXX

import Disruptor.RingBuffer.SingleProducer

------------------------------------------------------------------------

data EventProducer s = EventProducer
  { epWorker       :: s -> IO s
  , epInitialState :: s
  , epShutdown     :: Shutdown
  }

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

------------------------------------------------------------------------

newtype Shutdown = Shutdown (TMVar ())

newShutdownVar :: IO Shutdown
newShutdownVar = Shutdown <$> newEmptyTMVarIO

tellToShutdown :: Shutdown -> IO ()
tellToShutdown (Shutdown tmvar) = () <$ atomically (tryPutTMVar tmvar ())

shutdownProducer :: EventProducer s -> IO ()
shutdownProducer = tellToShutdown . epShutdown

isItTimeToShutdown :: Shutdown -> IO Bool
isItTimeToShutdown (Shutdown tmvar) = not <$> atomically (isEmptyTMVar tmvar)
