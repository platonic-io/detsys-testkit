{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE NumericUnderscores #-}

module Common where

import Control.Concurrent (getNumCapabilities)
import Control.Concurrent.Async
       (Async, async, cancel, link, mapConcurrently_)
import Control.Concurrent.MVar (MVar, newEmptyMVar, takeMVar)
import Control.Exception (assert, bracket)
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy.Char8 as LBS
import Data.Maybe (fromMaybe)
import Data.Time (UTCTime, diffUTCTime, getCurrentTime)
import Data.Word (Word64)
import GHC.Stats
       ( RTSStats(allocated_bytes, copied_bytes, max_mem_in_use_bytes)
       , getRTSStats
       , getRTSStatsEnabled
       )
import System.Mem (performMajorGC)
import System.Random (StdGen, mkStdGen, randomR)
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
nUM_OF_CLIENTS = 512

iTERATIONS :: Int
iTERATIONS = 200

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
  link a
  () <- takeMVar ready
  hc <- newHttpClient hOST pORT
  return (a, hc)

commonTeardown :: (Async (), HttpClient) -> IO ()
commonTeardown (a, _hc) = cancel a

commonBenchmark :: (Async (), HttpClient) -> IO ()
commonBenchmark (_a, hc) = do
  n <- getNumCapabilities
  printf "%-25.25s%10d\n" "CPU capabilities" n
  printf "%-25.25s%10d\n" "Total number of ops" (iTERATIONS * nUM_OF_CLIENTS)

  let gens = map mkStdGen [0 .. nUM_OF_CLIENTS - 1]

  performMajorGC

  !start <- getCurrentTime
  (startAllocs, startCopied, startMaxMemInUse) <- getAllocsAndCopied
  mapConcurrently_ (commonClient hc) gens
  !end <- getCurrentTime
  (endAllocs, endCopied, endMaxMemInUse) <- getAllocsAndCopied

  let duration :: Double
      !duration = realToFrac (diffUTCTime end start)

      throughput :: Double
      !throughput = realToFrac (iTERATIONS * nUM_OF_CLIENTS) / duration

  printf "%-25.25s%10.2f ops/s\n" "Throughput"     throughput
  printf "%-25.25s%10.2f s\n"     "Duration"       duration
  printf "%-25.25s%10s\n"         "Allocated mem"  (showBytes (endAllocs - startAllocs))
  printf "%-25.25s%10s\n"         "Copied mem"     (showBytes (endCopied - startCopied))
  printf "%-25.25s%10s\n"         "Max mem"        (showBytes
                                                     (max endMaxMemInUse startMaxMemInUse))

-- Stolen from `tasty-bench`.
showBytes :: Word64 -> String
showBytes i
  | t < 1000                 = printf "%3.0f B " t
  | t < 10189                = printf "%3.1f KB" (t / 1024)
  | t < 1023488              = printf "%3.0f KB" (t / 1024)
  | t < 10433332             = printf "%3.1f MB" (t / 1048576)
  | t < 1048051712           = printf "%3.0f MB" (t / 1048576)
  | t < 10683731149          = printf "%3.1f GB" (t / 1073741824)
  | t < 1073204953088        = printf "%3.0f GB" (t / 1073741824)
  | t < 10940140696372       = printf "%3.1f TB" (t / 1099511627776)
  | t < 1098961871962112     = printf "%3.0f TB" (t / 1099511627776)
  | t < 11202704073084108    = printf "%3.1f PB" (t / 1125899906842624)
  | t < 1125336956889202624  = printf "%3.0f PB" (t / 1125899906842624)
  | t < 11471568970838126592 = printf "%3.1f EB" (t / 1152921504606846976)
  | otherwise                = printf "%3.0f EB" (t / 1152921504606846976)
  where
    t = word64ToDouble i

    word64ToDouble :: Word64 -> Double
    word64ToDouble = fromIntegral

commonClient :: HttpClient -> StdGen -> IO ()
commonClient hc gen = go iTERATIONS 0 gen
  where
    go :: Int -> Int -> StdGen -> IO ()
    go 0 _maxIndex _gen = return ()
    go n  maxIndex  gen = do
      let (cmd, gen') = genCommand gen wRITE_FREQUENCY rEAD_FREQUENCY
      case cmd of
        Write -> do
          mMaxIndex' <- writeHttp hc vALUE_TO_WRITE
          go (n - 1) (fromMaybe maxIndex mMaxIndex') gen'
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

------------------------------------------------------------------------

getAllocsAndCopied :: IO (Word64, Word64, Word64)
getAllocsAndCopied = do
  b <- getRTSStatsEnabled
  if b
  then do
    s <- getRTSStats
    return (allocated_bytes s, copied_bytes s, max_mem_in_use_bytes s)
  else return (0, 0, 0)
