{-# LANGUAGE NumericUnderscores #-}

module Main where

import System.Mem (performGC)
import Control.Concurrent
import Control.Concurrent.Async
import Control.Concurrent.STM
import Data.IORef
import Data.Int
import Data.Time
import Text.Printf

import StuntDouble.Histogram.SingleProducer
import Disruptor.AtomicCounterPadded

------------------------------------------------------------------------

iTERATIONS :: Int64
iTERATIONS = 50_000_000

main :: IO ()
main = do
  n <- getNumCapabilities
  printf "%-25.25s%10d\n" "CPU capabilities" n
  queue <- newTBQueueIO (1024 * 64)
  histo <- newHistogram
  transactions <- newCounter 0

  let producer n | n == iTERATIONS = return ()
                 | otherwise       = do
        atomically (writeTBQueue queue n)
        {-# SCC "transactions+1" #-} incrCounter 1 transactions
        producer (n + 1)

      consumer = do
        n <- atomically (readTBQueue queue)
        t' <- {-# SCC "transactions-1" #-} decrCounter 1 transactions
        measureInt_ t' histo
        if n == iTERATIONS - 1
        then return ()
        else consumer

  performGC
  start <- getCurrentTime
  withAsync (producer 0) $ \ap ->
    withAsync consumer $ \ac -> do
       wait ap
       wait ac
       end <- getCurrentTime
       printf "%-25.25s%10d\n"     "Total number of events" iTERATIONS
       printf "%-25.25s%10.2f s\n" "Duration" (realToFrac (diffUTCTime end start) :: Double)
       let throughput :: Double
           throughput = realToFrac iTERATIONS / realToFrac (diffUTCTime end start)
       printf "%-25.25s%10.2f events/s\n" "Throughput" throughput
       meanTransactions <- hmean histo
       printf "%-25.25s%10.2f\n" "Mean concurrent txs" meanTransactions
       Just maxTransactions <- percentile 100.0 histo
       printf "%-25.25s%10.2f\n" "Max concurrent txs" maxTransactions
       printf "%-25.25s%10.2f ns\n" "Latency" ((meanTransactions / throughput) * 1000000)
