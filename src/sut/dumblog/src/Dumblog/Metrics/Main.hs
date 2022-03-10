{-# LANGUAGE NumericUnderscores #-}

module Dumblog.Metrics.Main where

import Control.Monad (forever)
import Control.Concurrent (threadDelay)
import Text.Printf (printf)
import GHC.IO.Encoding (setLocaleEncoding, utf8)

import Dumblog.Journal.Metrics
import Journal.Internal.Metrics

------------------------------------------------------------------------

metricsMain :: IO ()
metricsMain = forever $ do
  setLocaleEncoding utf8 -- Otherwise we can't print µ...
  metrics <- newMetrics dumblogSchema "/tmp/dumblog.metrics"
  putStrLn ansiClearScreen
  mMin  <- percentile metrics ServiceTime 0
  mMed  <- percentile metrics ServiceTime 50
  m90   <- percentile metrics ServiceTime 90
  m99   <- percentile metrics ServiceTime 99
  m999  <- percentile metrics ServiceTime 99.9
  m9999 <- percentile metrics ServiceTime 99.99
  mMax  <- percentile metrics ServiceTime 100
  putStrLn "Service time:"
  putStrLn (maybe "N/A" (printf "  min   %10.2f µs") mMin)
  putStrLn (maybe "N/A" (printf "  med   %10.2f µs") mMed)
  putStrLn (maybe "N/A" (printf "  90    %10.2f µs") m90)
  putStrLn (maybe "N/A" (printf "  99    %10.2f µs") m99)
  putStrLn (maybe "N/A" (printf "  99.9  %10.2f µs") m999)
  putStrLn (maybe "N/A" (printf "  99.99 %10.2f µs") m9999)
  putStrLn (maybe "N/A" (printf "  max   %10.2f µs") mMax)
  cnt <- count metrics ServiceTime
  putStrLn (printf "  count %10d" cnt)
  threadDelay 1_000_000

ansiClearScreen :: String
ansiClearScreen = "\ESC[2J"
