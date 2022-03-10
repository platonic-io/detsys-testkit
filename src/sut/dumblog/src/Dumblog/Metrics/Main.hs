{-# LANGUAGE NumericUnderscores #-}

module Dumblog.Metrics.Main where

import Control.Concurrent (threadDelay)
import Control.Exception (IOException)
import Control.Monad (forever)
import Data.Maybe (fromMaybe)
import GHC.IO.Encoding (setLocaleEncoding, utf8)
import Text.Printf (printf)

import Dumblog.Journal.Main
import Dumblog.Journal.Metrics
import Journal (journalMetadata)
import Journal.Internal.Metrics
import Journal.Types

------------------------------------------------------------------------

metricsMain :: IO ()
metricsMain = forever $ do
  setLocaleEncoding utf8 -- Otherwise we can't print µ...
  metrics <- newMetrics dumblogSchema dUMBLOG_METRICS
  eMeta <- journalMetadata dUMBLOG_JOURNAL dumblogOptions
  putStrLn ansiClearScreen
  displayServiceTime metrics
  displayQueueDepth eMeta
  threadDelay 1_000_000

ansiClearScreen :: String
ansiClearScreen = "\ESC[2J"

displayServiceTime :: DumblogMetrics -> IO ()
displayServiceTime metrics = do
  mMin  <- percentile metrics ServiceTime 0
  mMed  <- percentile metrics ServiceTime 50
  m90   <- percentile metrics ServiceTime 90
  m99   <- percentile metrics ServiceTime 99
  m999  <- percentile metrics ServiceTime 99.9
  m9999 <- percentile metrics ServiceTime 99.99
  mMax  <- percentile metrics ServiceTime 100
  putStrLn "Service time:"
  printf "  min   %10.2f µs\n" (fromMaybe 0 mMin)
  printf "  med   %10.2f µs\n" (fromMaybe 0 mMed)
  printf "  90    %10.2f µs\n" (fromMaybe 0 m90)
  printf "  99    %10.2f µs\n" (fromMaybe 0 m99)
  printf "  99.9  %10.2f µs\n" (fromMaybe 0 m999)
  printf "  99.99 %10.2f µs\n" (fromMaybe 0 m9999)
  printf "  max   %10.2f µs\n" (fromMaybe 0 mMax)
  cnt <- count metrics ServiceTime
  putStrLn (printf "  count %10d" cnt)

displayQueueDepth :: Either IOException Metadata -> IO ()
displayQueueDepth (Left _err) = do
  putStrLn "Journal metadata:"
  printf "  0 bytes produced\n"
  printf "  0 bytes consumed\n"
displayQueueDepth (Right meta) = do
  putStrLn "Journal metadata:"
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
