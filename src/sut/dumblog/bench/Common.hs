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
import System.Environment (getArgs)
import System.Mem (performMajorGC)
import System.Random (StdGen, mkStdGen, randomR)
import Text.Printf (printf)
import Text.Read (readMaybe)

import Dumblog.Common.HttpClient
import Dumblog.Common.Utils (showBytes)

------------------------------------------------------------------------

hOST :: String
hOST = "localhost"

pORT :: Int
pORT = 8054

wRITE_FREQUENCY :: Int
wRITE_FREQUENCY = 20

rEAD_FREQUENCY :: Int
rEAD_FREQUENCY = 80

iTERATIONS :: Int
iTERATIONS = 100000

vALUE_TO_WRITE :: ByteString
vALUE_TO_WRITE = LBS.pack "Dumblog"

bUFFER_CAPACITY :: Int
bUFFER_CAPACITY = 1024 * 64

------------------------------------------------------------------------

commonMain :: String -> (MVar () -> IO ()) -> IO ()
commonMain variant io = do
  args <- getArgs
  let clients = case args of
                  [s]        -> fromMaybe nUM_OF_CLIENTS (readMaybe s)
                  _otherwise -> nUM_OF_CLIENTS
  bracket (commonSetup variant io) commonTeardown (commonBenchmark clients)
  where
    nUM_OF_CLIENTS :: Int
    nUM_OF_CLIENTS = 100

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

(//) :: Int -> Int -> Int
i // j = round (realToFrac i / realToFrac j)

commonBenchmark :: Int -> (Async (), HttpClient) -> IO ()
commonBenchmark clients (_a, hc) = do
  n <- getNumCapabilities
  printf "%-25.25s%10d\n" "CPU capabilities"    n
  printf "%-25.25s%10d\n" "Number of clients"   clients
  printf "%-25.25s%10d\n" "Total number of ops" ((iTERATIONS // clients) * clients)

  let gens = map mkStdGen [0 .. clients - 1]

  performMajorGC

  !start <- getCurrentTime
  (startAllocs, startCopied, startMaxMemInUse) <- getAllocsAndCopied
  mapConcurrently_ (commonClient hc (iTERATIONS // clients)) gens
  !end <- getCurrentTime
  (endAllocs, endCopied, endMaxMemInUse) <- getAllocsAndCopied

  let duration :: Double
      !duration = realToFrac (diffUTCTime end start)

      throughput :: Double
      !throughput = realToFrac ((iTERATIONS // clients) * clients) / duration

  printf "%-25.25s%10.2f ops/s\n" "Throughput"     throughput
  printf "%-25.25s%10.2f s\n"     "Duration"       duration
  printf "%-25.25s%10s\n"         "Allocated mem"  (showBytes (endAllocs - startAllocs))
  printf "%-25.25s%10s\n"         "Copied mem"     (showBytes (endCopied - startCopied))
  printf "%-25.25s%10s\n"         "Max mem"        (showBytes
                                                     (max endMaxMemInUse startMaxMemInUse))

commonClient :: HttpClient -> Int -> StdGen -> IO ()
commonClient hc iterations gen = do
  mMaxIndex <- writeHttp hc vALUE_TO_WRITE
  let maxIndex = fromMaybe (error "commonClient: initial write failed...") mMaxIndex
  go (iterations - 1) maxIndex gen
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
