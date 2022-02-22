module Main where

import Control.Concurrent (getNumCapabilities)
import Control.Concurrent.Async
import Control.Concurrent.MVar
import Control.Exception (assert, bracket)
import Control.Monad
import Data.Time (UTCTime, diffUTCTime, getCurrentTime)
import System.Mem (performGC)
import System.Random
import Text.Printf (printf)

import Dumblog.Common.HttpClient
import Dumblog.SQLite.Main (sqliteDumblog)

import Common

------------------------------------------------------------------------

main :: IO ()
main = bracket setup teardown benchmark
  where
    setup :: IO (Async (), HttpClient)
    setup = do
      ready <- newEmptyMVar
      putStrLn "Starting Dumblog (SQLite)"
      a <- async (sqliteDumblog bUFFER_CAPACITY pORT (Just ready))
      () <- takeMVar ready
      hc <- newHttpClient hOST pORT
      return (a, hc)

    teardown :: (Async (), HttpClient) -> IO ()
    teardown (a, _hc) = cancel a

    benchmark :: (Async (), HttpClient) -> IO ()
    benchmark (_a, hc) = do
      n <- getNumCapabilities
      printf "%-25.25s%10d\n" "CPU capabilities" n
      printf "%-25.25s%10d\n" "Total number of ops" iTERATIONS

      let gens = map mkStdGen [0 .. nUM_OF_CLIENTS - 1]

      performGC

      start <- getCurrentTime
      mapConcurrently_ (client hc) gens
      end <- getCurrentTime

      let duration :: Double
          duration = realToFrac (diffUTCTime end start)

          throughput :: Double
          throughput = realToFrac iTERATIONS / duration

      printf "%-25.25s%10.2f ops/s\n" "Throughput" throughput
      printf "%-25.25s%10.2f s\n" "Duration" duration

    client :: HttpClient -> StdGen -> IO ()
    client hc gen = go iTERATIONS 0 gen
      where
        go :: Int -> Int -> StdGen -> IO ()
        go 0 _maxIndex _gen = return ()
        go n  maxIndex  gen = do
          let (cmd, gen') = genCommand gen wRITE_FREQUENCY rEAD_FREQUENCY
          case cmd of
            Write -> do
              maxIndex' <- writeHttp hc vALUE_TO_WRITE
              go (n - 1) maxIndex' gen'
            Read  ->
              let
                (ix, gen'') = randomR (0, maxIndex) gen'
              in do
                _mbs <- readHttp hc ix
                go (n - 1) maxIndex gen''

data Command = Write | Read

genCommand :: StdGen -> Int -> Int -> (Command, StdGen)
genCommand gen writeFreq readFreq =
  let
    (i, gen') = randomR (0, writeFreq + readFreq) gen

    cmd | i <= writeFreq = Write
        | otherwise      = assert (writeFreq < i &&
                                   i <= writeFreq + readFreq) Read
  in
    (cmd, gen')
