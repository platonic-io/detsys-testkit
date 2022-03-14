{-# LANGUAGE NoOverloadedStrings #-}
{-# LANGUAGE NumericUnderscores #-}

module Dumblog.Metrics.Main where

import Control.Concurrent (threadDelay)
import Control.Exception (IOException)
import Data.Maybe (fromMaybe)
import Data.Time (UTCTime, diffUTCTime, getCurrentTime)
import GHC.IO.Encoding (setLocaleEncoding, utf8)
import System.Directory (removePathForcibly)
import Text.Printf (printf)

import Dumblog.Journal.Main
import Dumblog.Journal.Metrics
import Journal (journalMetadata)
import Journal.Internal.Metrics hiding (Latency)
import Journal.Types

------------------------------------------------------------------------

data ThroughputState = ThroughputState
  { tsLastTotalCount :: !Int
  , tsLastTime       :: !UTCTime
  , tsIterations     :: !Int
  , tsSum            :: !Double
  }

initThroughputState :: IO ThroughputState
initThroughputState = do
  now <- getCurrentTime
  return (ThroughputState 0 now 0 0.0)

throughputAvg :: ThroughputState -> Double
throughputAvg ts = tsSum ts / realToFrac (tsIterations ts)

metricsMain :: IO ()
metricsMain = do
  setLocaleEncoding utf8 -- Otherwise we can't print µ...
  removePathForcibly dUMBLOG_METRICS
  ts <- initThroughputState
  go ts
  where
    go :: ThroughputState -> IO ()
    go ts = do
      metrics <- newMetrics dumblogSchema dUMBLOG_METRICS
      eMeta   <- journalMetadata dUMBLOG_JOURNAL dumblogOptions

      -- Only needed on MacOS it seems.
      msyncMetrics metrics
      either (const (return ())) msyncMetadata eMeta

      putStr (ansiClearScreen ++ ansiGoto 1 1)
      displayLatencyAndServiceTime metrics
      displayQueueDepth metrics
      ts' <- displayThroughput metrics ts
      displayUtilisation metrics ts'
      displayJournalMetadata eMeta
      displayConcurrentConnections metrics
      displayErrors metrics

      threadDelay 1_000_000
      go ts'

ansiClearScreen :: String
ansiClearScreen = "\ESC[2J"

ansiGoto :: Int -> Int -> String
ansiGoto x y    = "\ESC[" ++ show y ++ ";" ++ show x ++ "H"

displayLatencyAndServiceTime :: DumblogMetrics -> IO ()
displayLatencyAndServiceTime metrics = do
  mMinL  <- percentile metrics Latency 0
  mMedL  <- percentile metrics Latency 50
  m90L   <- percentile metrics Latency 90
  m99L   <- percentile metrics Latency 99
  m999L  <- percentile metrics Latency 99.9
  m9999L <- percentile metrics Latency 99.99
  mMaxL  <- percentile metrics Latency 100
  mMin  <- percentile metrics ServiceTimeWrites 0
  mMed  <- percentile metrics ServiceTimeWrites 50
  m90   <- percentile metrics ServiceTimeWrites 90
  m99   <- percentile metrics ServiceTimeWrites 99
  m999  <- percentile metrics ServiceTimeWrites 99.9
  m9999 <- percentile metrics ServiceTimeWrites 99.99
  mMax  <- percentile metrics ServiceTimeWrites 100
  mMin'  <- percentile metrics ServiceTimeReads 0
  mMed'  <- percentile metrics ServiceTimeReads 50
  m90'   <- percentile metrics ServiceTimeReads 90
  m99'   <- percentile metrics ServiceTimeReads 99
  m999'  <- percentile metrics ServiceTimeReads 99.9
  m9999' <- percentile metrics ServiceTimeReads 99.99
  mMax'  <- percentile metrics ServiceTimeReads 100
  printf "%-25.25s%-25.25s%-25.25s\n" "\nLatency:"
                                       "Service time (writes):"
                                       "Service time (reads):"
  printf "  min   %10.2f µs%15.2f µs%20.2f µs\n"
    (fromMaybe 0 mMinL) (fromMaybe 0 mMin)  (fromMaybe 0 mMin')
  printf "  med   %10.2f µs%15.2f µs%20.2f µs\n"
    (fromMaybe 0 mMedL) (fromMaybe 0 mMed)  (fromMaybe 0 mMed')
  printf "  90    %10.2f µs%15.2f µs%20.2f µs\n"
    (fromMaybe 0 m90L) (fromMaybe 0 m90)   (fromMaybe 0 m90')
  printf "  99    %10.2f µs%15.2f µs%20.2f µs\n"
    (fromMaybe 0 m99L) (fromMaybe 0 m99)   (fromMaybe 0 m99')
  printf "  99.9  %10.2f µs%15.2f µs%20.2f µs\n"
    (fromMaybe 0 m999L) (fromMaybe 0 m999)  (fromMaybe 0 m999')
  printf "  99.99 %10.2f µs%15.2f µs%20.2f µs\n"
    (fromMaybe 0 m9999L) (fromMaybe 0 m9999) (fromMaybe 0 m9999')
  printf "  max   %10.2f µs%15.2f µs%20.2f µs\n"
    (fromMaybe 0 mMaxL) (fromMaybe 0 mMax)  (fromMaybe 0 mMax')
  latencySum <- realToFrac <$> metricsSum metrics Latency :: IO Double
  writeSum <- realToFrac <$> metricsSum metrics ServiceTimeWrites :: IO Double
  readSum  <- realToFrac <$> metricsSum metrics ServiceTimeReads  :: IO Double
  latencyCnt <- count metrics Latency
  writeCnt <- count metrics ServiceTimeWrites
  readCnt  <- count metrics ServiceTimeReads
  printf "  sum   %10.2f s %15.2f s %20.2f s\n"
    (latencySum / 1e6) (writeSum / 1e6) (readSum / 1e6)
  let totalCnt :: Double
      totalCnt = realToFrac (writeCnt + readCnt)
  printf "  count %7d %17d (%2.0f%%) %15d (%2.0f%%)\n"
    latencyCnt
    writeCnt (realToFrac writeCnt / totalCnt * 100)
    readCnt  (realToFrac readCnt  / totalCnt * 100)

displayQueueDepth :: DumblogMetrics -> IO ()
displayQueueDepth metrics = do
  putStr "\nSaturation (queue depth):"
  depth <- getCounter metrics QueueDepth
  printf " %d\n" depth

displayThroughput :: DumblogMetrics -> ThroughputState -> IO ThroughputState
displayThroughput metrics ts = do
  now <- getCurrentTime
  writeCnt <- count metrics ServiceTimeWrites
  readCnt  <- count metrics ServiceTimeReads

  let totalCnt :: Int
      totalCnt = writeCnt + readCnt

      throughput :: Double
      throughput = realToFrac (totalCnt - tsLastTotalCount ts)
                 / realToFrac (diffUTCTime now (tsLastTime ts))

      ts' :: ThroughputState
      ts' = ThroughputState totalCnt now (tsIterations ts + 1) (tsSum ts + throughput)

  printf "\nThroughput: %.2f ops/s\n" throughput
  printf "Throughput (avg): %.2f ops/s\n" (throughputAvg ts')
  return ts'

displayJournalMetadata :: Either IOException Metadata -> IO ()
displayJournalMetadata (Left _err) = do
  putStrLn "\nJournal metadata:"
  printf "  0 bytes produced\n"
  printf "  0 bytes consumed\n"
  printf "  0 bytes difference\n"
displayJournalMetadata (Right meta) = do
  putStrLn "\nJournal metadata:"
  termCount <- activeTermCount meta
  let index = indexByTermCount termCount
  rt <- readRawTail meta index
  initTermId <- readInitialTermId meta
  termLen <- readTermLength meta

  let termId            = rawTailTermId rt
      termOffset        = rawTailTermOffset rt termLen
      termBeginPosition =
        computeTermBeginPosition termId (positionBitsToShift termLen) initTermId

      produced = termBeginPosition + fromIntegral termOffset
  consumed <- readBytesConsumed meta Sub1
  printf "  %d bytes produced\n" produced
  printf "  %d bytes consumed\n" consumed
  printf "  %d bytes difference\n" (produced - fromIntegral consumed)

displayConcurrentConnections :: DumblogMetrics -> IO ()
displayConcurrentConnections metrics = do
  putStr "\nConcurrent number of transactions:"
  cnt <- getCounter metrics CurrentNumberTransactions
  printf " %d\n" cnt

displayErrors :: DumblogMetrics -> IO ()
displayErrors metrics = do
  putStr "\nErrors:"
  errors <- getCounter metrics ErrorsEncountered
  printf " %d\n" errors

displayUtilisation :: DumblogMetrics -> ThroughputState -> IO ()
displayUtilisation metrics ts = do
  serviceTimeWAvg <- metricsAvg metrics ServiceTimeWrites
  serviceTimeRAvg <- metricsAvg metrics ServiceTimeReads
  printf "\nUtilisation: %.2f\n"
    -- Throughput uses seconds and service time uses µs, hence the `* 10^-6`.
    (throughputAvg ts * (serviceTimeWAvg + serviceTimeRAvg) * 1e-6)
