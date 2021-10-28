{-# LANGUAGE NumericUnderscores #-}

module Main where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (wait, withAsync)
import Control.Monad (replicateM_)
import Data.Time (getCurrentTime, diffUTCTime)
import System.Mem (performGC)
import Text.Printf (printf)

import Control.Concurrent.Chan.Unagi.NoBlocking.Unboxed

------------------------------------------------------------------------

iTERATIONS :: Int
iTERATIONS = 100_000_000

main :: IO ()
main = do
  (i, o) <- newChan

  let producer = replicateM_ iTERATIONS (writeChan i (1 :: Int))
      consumer = replicateM_ iTERATIONS (readChan (threadDelay 1) o)

  performGC
  start <- getCurrentTime

  withAsync producer $ \ap ->
    withAsync consumer $ \ac -> do
      wait ap
      wait ac
      end <- getCurrentTime

      let duration :: Double
          duration = realToFrac (diffUTCTime end start)

          throughput :: Double
          throughput = realToFrac iTERATIONS / duration

      printf "%-25.25s%10.2f events/s\n" "Throughput" throughput
      printf "%-25.25s%10.2f s\n" "Duration" duration
