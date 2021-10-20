module Main where

import Control.Concurrent
import Control.Concurrent.Async
import Data.Time
import Data.IORef
import Data.Atomics.Counter
import Data.Int
import Text.Printf

import Disruptor
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
  transactions <- newCounter (0 :: Int)

  let production () = do
        {-# SCC "transactions+1" #-} incrCounter_ 1 transactions
        return (1 :: Int, ())
      backPressure () = return ()
  ep <- newEventProducer rb production backPressure ()
  let handler _s _n snr _endOfBatch = do
        t' <- {-# SCC "transactions-1" #-} incrCounter (-1) transactions
        measureInt_ t' histo
        return snr

  ec <- newEventConsumer rb handler 0 [] (Sleep 1)
  setGatingSequences rb [Exists ec]

  let areWeDoneConsuming = do
        t <- readIORef (ecSequenceNumber ec)
        if t >= fromIntegral iTERATIONS - 1
        then return ()
        else do
          threadDelay 10000
          areWeDoneConsuming
  start <- getCurrentTime
  withEventProducer ep $ \aep ->
    withEventConsumer ec $ \aec ->
     withAsync areWeDoneConsuming $ \a -> do
       wait a
       shutdownProducer ep
       wait aep
       shutdownConsumer ec
       events <- wait aec
       end <- getCurrentTime
       printf "%-25.25s%10d\n"     "Total number of events" (getSequenceNumber events)
       printf "%-25.25s%10.2f s\n" "Duration" (realToFrac (diffUTCTime end start) :: Double)
       let throughput = realToFrac events / realToFrac (diffUTCTime end start)
       printf "%-25.25s%10.2f events/s\n" "Throughput" throughput
       meanTransactions <- hmean histo
       printf "%-25.25s%10.2f\n" "Mean concurrent txs" meanTransactions
       Just maxTransactions <- percentile 100.0 histo
       printf "%-25.25s%10.2f\n" "Max concurrent txs" maxTransactions
       printf "%-25.25s%10.2f ns\n" "Latency" ((meanTransactions / throughput) * 1000000)
