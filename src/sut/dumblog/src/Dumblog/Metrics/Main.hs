{-# LANGUAGE NumericUnderscores #-}

module Dumblog.Metrics.Main where

import Control.Concurrent (threadDelay)
import Control.Exception (IOException)
import Control.Monad (forever)
import Data.Maybe (fromMaybe)
import GHC.IO.Encoding (setLocaleEncoding, utf8)
import Text.Printf (printf)
import Data.Time (UTCTime, getCurrentTime, diffUTCTime)

import Dumblog.Journal.Main
import Dumblog.Journal.Metrics
import Journal (journalMetadata)
import Journal.Internal.Metrics
import Journal.Types

------------------------------------------------------------------------

metricsMain :: IO ()
metricsMain = do
  startTime <- getCurrentTime
  forever $ do
    setLocaleEncoding utf8 -- Otherwise we can't print µ...
    metrics <- newMetrics dumblogSchema dUMBLOG_METRICS
    eMeta <- journalMetadata dUMBLOG_JOURNAL dumblogOptions
    putStrLn ansiClearScreen
    displayServiceTime metrics
    displayThroughput metrics startTime
    displayJournalMetadata eMeta
    displayConcurrentConnections metrics
    threadDelay 1_000_000

ansiClearScreen :: String
ansiClearScreen = "\ESC[2J"

displayServiceTime :: DumblogMetrics -> IO ()
displayServiceTime metrics = do
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
  printf "%-25.25s%-70.25s\n" "Service time (writes):" "Service time (reads):"
  printf "  min   %10.2f µs%15.2fµs\n" (fromMaybe 0 mMin)  (fromMaybe 0 mMin')
  printf "  med   %10.2f µs%15.2fµs\n" (fromMaybe 0 mMed)  (fromMaybe 0 mMed')
  printf "  90    %10.2f µs%15.2fµs\n" (fromMaybe 0 m90)   (fromMaybe 0 m90')
  printf "  99    %10.2f µs%15.2fµs\n" (fromMaybe 0 m99)   (fromMaybe 0 m99')
  printf "  99.9  %10.2f µs%15.2fµs\n" (fromMaybe 0 m999)  (fromMaybe 0 m999')
  printf "  99.99 %10.2f µs%15.2fµs\n" (fromMaybe 0 m9999) (fromMaybe 0 m9999')
  printf "  max   %10.2f µs%15.2fµs\n" (fromMaybe 0 mMax)  (fromMaybe 0 mMax')
  writeCnt <- count metrics ServiceTimeWrites
  readCnt  <- count metrics ServiceTimeReads
  let totalCnt :: Double
      totalCnt = realToFrac (writeCnt + readCnt)
  printf "  count %7d (%2.0f%%) %10d (%2.0f%%)\n"
    writeCnt (realToFrac writeCnt / totalCnt * 100)
    readCnt  (realToFrac readCnt  / totalCnt * 100)

displayThroughput :: DumblogMetrics -> UTCTime -> IO ()
displayThroughput metrics startTime = do
  now <- getCurrentTime
  writeCnt <- count metrics ServiceTimeWrites
  readCnt  <- count metrics ServiceTimeReads
  let totalCnt :: Double
      totalCnt = realToFrac (writeCnt + readCnt)
  printf "\nThroughput: %.2f ops/s\n" (totalCnt / realToFrac (diffUTCTime now startTime))

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
