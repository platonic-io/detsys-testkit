module Main where

import Control.Concurrent
import Control.Concurrent.Async
import Control.Concurrent.MVar
import Control.Monad
import Data.Atomics.Counter
import Data.IORef
import Data.Int
import Data.Time
import Text.Printf

import Disruptor
import Disruptor.SequenceNumber
import StuntDouble.Histogram.SingleProducer

------------------------------------------------------------------------

iTERATIONS :: Int64
iTERATIONS = 1000 * 1000 * 50

main :: IO ()
main = do
  n <- getNumCapabilities
  printf "%-25.25s%10d\n" "CPU capabilities" n
  let ringBufferCapacity = 1024 * 64
  rb <- newRingBuffer SingleProducer ringBufferCapacity
  histo <- newHistogram
  transactions <- newCounter 0
  consumerFinished <- newEmptyMVar

  let ep = EventProducer (const (go iTERATIONS)) () (error "not used")
        where
          go :: Int64 -> IO ()
          go 0 = return ()
          go n = do
            {-# SCC "transactions+1" #-} incrCounter_ 1 transactions
            mSnr <- tryNext rb
            case mSnr of
              Just snr -> do
                set rb snr (1 :: Int)
                publish rb snr
                go (n - 1)
              Nothing -> go n

  let handler _s _n snr endOfBatch = do
        t' <- {-# SCC "transactions-1" #-} incrCounter (-1) transactions
        measureInt_ t' histo
        when (endOfBatch && getSequenceNumber snr == iTERATIONS - 1) $
          putMVar consumerFinished ()
        return ()

  ec <- newEventConsumer rb handler () [] (Sleep 1)
  setGatingSequences rb [Exists ec]

  start <- getCurrentTime
  withEventProducer ep $ \aep ->
    withEventConsumer ec $ \aec -> do
      () <- takeMVar consumerFinished
      end <- getCurrentTime
      cancel aep
      cancel aec
      printf "%-25.25s%10d\n"     "Total number of events" iTERATIONS
      printf "%-25.25s%10.2f s\n" "Duration" (realToFrac (diffUTCTime end start) :: Double)
      let throughput = realToFrac iTERATIONS / realToFrac (diffUTCTime end start)
      printf "%-25.25s%10.2f events/s\n" "Throughput" throughput
      meanTransactions <- hmean histo
      printf "%-25.25s%10.2f\n" "Mean concurrent txs" meanTransactions
      Just maxTransactions <- percentile 100.0 histo
      printf "%-25.25s%10.2f\n" "Max concurrent txs" maxTransactions
      printf "%-25.25s%10.2f ns\n" "Latency" ((meanTransactions / throughput) * 1000000)
