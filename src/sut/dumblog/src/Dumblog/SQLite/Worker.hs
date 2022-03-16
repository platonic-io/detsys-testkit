{-# LANGUAGE BangPatterns #-}

module Dumblog.SQLite.Worker where

import Control.Concurrent (threadDelay)
import Control.Concurrent.MVar (putMVar)
import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TBQueue
       (TBQueue, flushTBQueue, readTBQueue)
import Control.Monad (when)
import qualified Data.ByteString.Lazy.Char8 as LBS

import Journal.Internal.Metrics hiding (Latency)
import Dumblog.Common.Metrics
import Dumblog.SQLite.Command
import Dumblog.SQLite.DB

------------------------------------------------------------------------

worker :: TBQueue Command -> DumblogMetrics -> Connection -> IO ()
worker queue metrics conn = go
  where
    go :: IO ()
    go = do
      cmd <- atomically (readTBQueue queue)
      decrCounter_ metrics QueueDepth 1
      !startTime <- getCurrentNanosSinceEpoch
      execute conn cmd
      !endTime <- getCurrentNanosSinceEpoch

      -- Convert from nano s to µs with `* 10^-3`.
      let arrivalTime = commandArrivalTime cmd
          latency     = realToFrac ((startTime - arrivalTime)) * 0.001 -- µs.
          serviceTime = realToFrac ((endTime   - startTime))   * 0.001
      measure metrics Latency latency
      measure metrics (case cmd of
                         Write {} -> ServiceTimeWrites
                         Read {}  -> ServiceTimeReads) serviceTime
      measure metrics ResponseTime (latency + serviceTime)
      case cmd of
        Write bs _ _ -> measure metrics WriteSize (realToFrac (LBS.length bs))
        _otherwise   -> return ()
      go

_batchingWorker :: TBQueue Command -> Connection -> IO ()
_batchingWorker queue conn = go
  where
    go :: IO ()
    go = do
      cmds <- atomically (flushTBQueue queue)
      mapM_ (execute conn) cmds
      when (null cmds) (threadDelay 0)
      go

execute :: Connection -> Command -> IO ()
execute conn (Read ix _arrivalTime response) = do
  bs <- readDB conn ix
  putMVar response bs
execute conn (Write bs _arrivalTime response) = do
  ix <- writeDB conn bs
  putMVar response ix
