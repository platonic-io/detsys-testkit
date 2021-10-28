{-# LANGUAGE NumericUnderscores #-}

module Main where

import Control.Concurrent
import Control.Concurrent.Async
import Control.Concurrent.MVar
import Control.Monad
import Data.IORef
import Data.Int
import Data.Time
import System.Mem (performGC)
import Text.Printf

import Disruptor.MP.Consumer
import Disruptor.MP.Producer
import Disruptor.MP.RingBuffer
import Disruptor.SequenceNumber
import Disruptor.AtomicCounterPadded
import StuntDouble.Histogram.SingleProducer

------------------------------------------------------------------------

iTERATIONS :: Int64
iTERATIONS = 100_000_000

main :: IO ()
main = do
  n <- getNumCapabilities
  printf "%-25.25s%10d\n" "CPU capabilities" n
  printf "%-25.25s%10d\n" "Total number of events" iTERATIONS
  mapM_ (\i -> printf "%s %d:\n" "Run" i >> once) [(0 :: Int)..0]

once :: IO ()
once = do
  let ringBufferCapacity = 1024 * 64
  rb <- newRingBuffer ringBufferCapacity
  -- histo <- newHistogram
  -- transactions <- newCounter 0
  consumerFinished <- newEmptyMVar

  let ep = EventProducer (const (go iTERATIONS)) ()
        where
          go :: Int64 -> IO ()
          go 0 = return ()
          go n = blocking n
            where
              blocking n = do
                snr <- next rb
                -- {-# SCC "transactions+1" #-} incrCounter_ 1 transactions
                {-# SCC set #-} set rb snr (1 :: Int)
                {-# SCC publish #-} publish rb snr
                go (n - 1)

              nonBlocking n = do
                mSnr <- tryNext rb
                case mSnr of
                  Some snr -> do
                    -- {-# SCC "transactions+1" #-} incrCounter_ 1 transactions
                    set rb snr (1 :: Int)
                    publish rb snr
                    go (n - 1)
                  None -> do
                    threadDelay 1
                    go n

  let handler _s _n snr endOfBatch = do
        -- t' <- {-# SCC "transactions-1" #-} decrCounter 1 transactions
        -- measureInt_ t' histo
        when (endOfBatch && getSequenceNumber snr == (iTERATIONS * 3) - 1) $
          putMVar consumerFinished ()
        return ()

  ec <- newEventConsumer rb handler () [] (Sleep 1)
  setGatingSequences rb [ecSequenceNumber ec]

  performGC
  start <- getCurrentTime
  withEventProducer ep $ \aep1 ->
    withEventProducer ep $ \aep2 ->
      withEventProducer ep $ \aep3 ->
        withEventConsumer ec $ \aec -> do
          () <- takeMVar consumerFinished
          end <- getCurrentTime
          cancel aep1
          cancel aep2
          cancel aep3
          cancel aec
          let duration :: Double
              duration = realToFrac (diffUTCTime end start)

              throughput :: Double
              throughput = realToFrac iTERATIONS / duration
          printf "%-25.25s%10.2f events/s\n" "Throughput" throughput
          printf "%-25.25s%10.2f s\n" "Duration" duration
          -- XXX: prettyPrintHistogram histo
          -- meanTransactions <- hmean histo
          -- printf "%-25.25s%10.2f\n" "Mean concurrent txs" meanTransactions
          -- Just maxTransactions <- percentile 100.0 histo
          -- printf "%-25.25s%10.2f\n" "Max concurrent txs" maxTransactions
          -- printf "%-25.25s%10.2f ns\n" "Latency" ((meanTransactions / throughput) * 1000000)
