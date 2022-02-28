{-# LANGUAGE NumericUnderscores #-}

module Common where

import Control.Concurrent (getNumCapabilities)
import Control.Concurrent.MVar (MVar, newEmptyMVar, takeMVar)
import Control.Concurrent.Async (Async, async, cancel, mapConcurrently_)
import Control.Exception (assert, bracket)
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy.Char8 as LBS
import Data.Time (UTCTime, diffUTCTime, getCurrentTime)
import System.Mem (performMajorGC)
import System.Random (StdGen, randomR, mkStdGen)
import Text.Printf (printf)

import Dumblog.Common.HttpClient

------------------------------------------------------------------------

hOST :: String
hOST = "localhost"

pORT :: Int
pORT = 8054

wRITE_FREQUENCY :: Int
wRITE_FREQUENCY = 20

rEAD_FREQUENCY :: Int
rEAD_FREQUENCY = 80

nUM_OF_CLIENTS :: Int
nUM_OF_CLIENTS = 3

iTERATIONS :: Int
iTERATIONS = 10_000

vALUE_TO_WRITE :: ByteString
vALUE_TO_WRITE = LBS.pack "Dumblog"

bUFFER_CAPACITY :: Int
bUFFER_CAPACITY = 1024 * 64

------------------------------------------------------------------------

commonMain :: String -> (MVar () -> IO ()) -> IO ()
commonMain variant io =
  bracket (commonSetup variant io) commonTeardown commonBenchmark

commonSetup :: String -> (MVar () -> IO ()) -> IO (Async (), HttpClient)
commonSetup msg io = do
  ready <- newEmptyMVar
  putStrLn ("Starting Dumblog (" ++ msg ++ ")")
  a <- async (io ready)
  () <- takeMVar ready
  hc <- newHttpClient hOST pORT
  return (a, hc)

commonTeardown :: (Async (), HttpClient) -> IO ()
commonTeardown (a, _hc) = cancel a

commonBenchmark :: (Async (), HttpClient) -> IO ()
commonBenchmark (_a, hc) = do
  n <- getNumCapabilities
  printf "%-25.25s%10d\n" "CPU capabilities" n
  printf "%-25.25s%10d\n" "Total number of ops" iTERATIONS

  let gens = map mkStdGen [0 .. nUM_OF_CLIENTS - 1]

  performMajorGC

  start <- getCurrentTime
  mapConcurrently_ (commonClient hc) gens
  end <- getCurrentTime

  let duration :: Double
      duration = realToFrac (diffUTCTime end start)

      throughput :: Double
      throughput = realToFrac iTERATIONS / duration

  printf "%-25.25s%10.2f ops/s\n" "Throughput" throughput
  printf "%-25.25s%10.2f s\n"     "Duration"   duration

commonClient :: HttpClient -> StdGen -> IO ()
commonClient hc gen = go iTERATIONS 0 gen
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
