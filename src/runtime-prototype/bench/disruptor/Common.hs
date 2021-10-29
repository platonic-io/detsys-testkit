{-# LANGUAGE NumericUnderscores #-}

module Common where

import Control.Concurrent
import Control.Concurrent.Async
import Control.Concurrent.MVar
import Data.Time
import System.Mem (performGC)
import Text.Printf

------------------------------------------------------------------------

iTERATIONS :: Int
iTERATIONS = 100_000_000

-- Single-producer single-consumer helper.
spsc :: IO a -> (a -> IO ()) -> (a -> MVar () -> IO ()) -> IO ()
spsc setup producer consumer = do
  n <- getNumCapabilities
  printf "%-25.25s%10d\n" "CPU capabilities" n
  printf "%-25.25s%10d\n" "Total number of events" iTERATIONS
  r <- setup
  consumerFinished <- newEmptyMVar
  performGC
  start <- getCurrentTime
  withAsync (producer r) $ \ap ->
    withAsync (consumer r consumerFinished) $ \ac -> do
      () <- takeMVar consumerFinished
      end <- getCurrentTime
      cancel ap
      cancel ac
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
