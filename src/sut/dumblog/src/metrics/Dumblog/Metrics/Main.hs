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

import Dumblog.Common.Constants (dumblogJournalPath, dumblogOptions)
import Dumblog.Common.Metrics
import Dumblog.Common.Utils (showBytes)
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

metricsMain :: Int -> IO ()
metricsMain port = do
  setLocaleEncoding utf8 -- Otherwise we can't print µ...
  -- removePathForcibly (dumblogMetricsPath port)
  ts <- initThroughputState
  go ts
  where
    go :: ThroughputState -> IO ()
    go ts = do
      metrics <- newMetrics dumblogSchema (dumblogMetricsPath port)
      eMeta   <- journalMetadata (dumblogJournalPath port) dumblogOptions

      -- Only needed on MacOS it seems.
      msyncMetrics metrics
      either (const (return ())) msyncMetadata eMeta

      putStr (ansiClearScreen ++ ansiGoto 1 1)
      displayTimings metrics
      displayQueueDepth metrics
      ts' <- displayThroughput metrics ts
      displayUtilisation metrics ts'
      displayJournalMetadata eMeta
      displayWriteSize metrics
      displayConcurrentConnections metrics
      displayErrors metrics
      displayMetricsSize

      threadDelay 1_000_000
      go ts'

ansiClearScreen :: String
ansiClearScreen = "\ESC[2J"

ansiGoto :: Int -> Int -> String
ansiGoto x y    = "\ESC[" ++ show y ++ ";" ++ show x ++ "H"

displayTimings :: DumblogMetrics -> IO ()
displayTimings metrics = do
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
  mMinRT  <- percentile metrics ResponseTime 0
  mMedRT  <- percentile metrics ResponseTime 50
  m90RT   <- percentile metrics ResponseTime 90
  m99RT   <- percentile metrics ResponseTime 99
  m999RT  <- percentile metrics ResponseTime 99.9
  m9999RT <- percentile metrics ResponseTime 99.99
  mMaxRT  <- percentile metrics ResponseTime 100
  printf "\n%-13.13s%-12.12s%-25.25s%-25.25s%-25.25s\n" "" "Latency:"
                                       "Service time (writes):"
                                       "Service time (reads):"
                                       "Response time:"
  printf "  min   %10.2f µs%15.2f µs%20.2f µs%25.2f µs\n"
    (fromMaybe 0 mMinL) (fromMaybe 0 mMin)  (fromMaybe 0 mMin') (fromMaybe 0 mMinRT)
  printf "  med   %10.2f µs%15.2f µs%20.2f µs%25.2f µs\n"
    (fromMaybe 0 mMedL) (fromMaybe 0 mMed)  (fromMaybe 0 mMed') (fromMaybe 0 mMedRT)
  printf "  90    %10.2f µs%15.2f µs%20.2f µs%25.2f µs\n"
    (fromMaybe 0 m90L) (fromMaybe 0 m90)   (fromMaybe 0 m90') (fromMaybe 0 m90RT)
  printf "  99    %10.2f µs%15.2f µs%20.2f µs%25.2f µs\n"
    (fromMaybe 0 m99L) (fromMaybe 0 m99)   (fromMaybe 0 m99') (fromMaybe 0 m99RT)
  printf "  99.9  %10.2f µs%15.2f µs%20.2f µs%25.2f µs\n"
    (fromMaybe 0 m999L) (fromMaybe 0 m999)  (fromMaybe 0 m999') (fromMaybe 0 m999RT)
  printf "  99.99 %10.2f µs%15.2f µs%20.2f µs%25.2f µs\n"
    (fromMaybe 0 m9999L) (fromMaybe 0 m9999) (fromMaybe 0 m9999') (fromMaybe 0 m9999RT)
  printf "  max   %10.2f µs%15.2f µs%20.2f µs%25.2f µs\n"
    (fromMaybe 0 mMaxL) (fromMaybe 0 mMax)  (fromMaybe 0 mMax') (fromMaybe 0 mMaxRT)
  latencySum <- realToFrac <$> metricsSum metrics Latency :: IO Double
  writeSum <- realToFrac <$> metricsSum metrics ServiceTimeWrites :: IO Double
  readSum  <- realToFrac <$> metricsSum metrics ServiceTimeReads  :: IO Double
  respTimeSum  <- realToFrac <$> metricsSum metrics ResponseTime  :: IO Double
  latencyCnt <- count metrics Latency
  writeCnt <- count metrics ServiceTimeWrites
  readCnt  <- count metrics ServiceTimeReads
  respTimeCnt  <- count metrics ResponseTime
  printf "  sum   %10.2f s %15.2f s %20.2f s %25.2f s\n"
    (latencySum / 1e6) (writeSum / 1e6) (readSum / 1e6) (respTimeSum / 1e6)
  let totalCnt :: Double
      totalCnt = realToFrac (writeCnt + readCnt)
  printf "  count %7d %17d (%2.0f%%) %16d (%2.0f%%) %21d\n"
    latencyCnt
    writeCnt (realToFrac writeCnt / totalCnt * 100)
    readCnt  (realToFrac readCnt  / totalCnt * 100)
    respTimeCnt

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

  printf "\nThroughput: %.2f ops/s (%.2f avg ops/s)\n"
    throughput (throughputAvg ts')

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
  if termLen == 0
  then return ()
  else do
    let termId            = rawTailTermId rt
        termOffset        = rawTailTermOffset rt termLen
        termBeginPosition =
          computeTermBeginPosition termId (positionBitsToShift termLen) initTermId

        produced = termBeginPosition + fromIntegral termOffset
    consumed <- readBytesConsumed meta Sub1
    printf "  %d bytes produced\n" produced
    printf "  %d bytes consumed\n" consumed
    printf "  %d bytes difference\n" (produced - fromIntegral consumed)

displayWriteSize :: DumblogMetrics -> IO ()
displayWriteSize metrics = do
  mMin  <- percentile metrics WriteSize 0
  mMed  <- percentile metrics WriteSize 50
  mMax  <- percentile metrics WriteSize 100
  printf "\nWrite size, "
  printf "min:   %10.0f bytes, med:  %10.0f bytes, max:  %10.0f bytes\n"
    (fromMaybe 0 mMin) (fromMaybe 0 mMed)  (fromMaybe 0 mMax)

displayConcurrentConnections :: DumblogMetrics -> IO ()
displayConcurrentConnections metrics = do
  putStr "\nConcurrent number of transactions:"
  cnt <- getCounter metrics CurrentNumberTransactions
  printf " %d\n" cnt

displayUtilisation :: DumblogMetrics -> ThroughputState -> IO ()
displayUtilisation metrics ts = do
  serviceTimeWAvg <- metricsAvg metrics ServiceTimeWrites
  serviceTimeRAvg <- metricsAvg metrics ServiceTimeReads
  printf "\nUtilisation: %.2f\n"
    -- Throughput uses seconds and service time uses µs, hence the `* 10^-6`.
    (throughputAvg ts * (serviceTimeWAvg + serviceTimeRAvg) * 1e-6)

displayErrors :: DumblogMetrics -> IO ()
displayErrors metrics = do
  putStr "\nErrors:"
  errors <- getCounter metrics ErrorsEncountered
  printf " %d\n" errors

displayMetricsSize :: IO ()
displayMetricsSize =
  printf "\n(Metrics' memory use: %s)\n" (showBytes (fromIntegral (metricSize dumblogSchema)))
