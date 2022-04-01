module Dumblog.SQLite.Main where

import Control.Concurrent (MVar)
import Control.Concurrent.Async (withAsync)
import Control.Concurrent.STM.TBQueue (newTBQueueIO)
import Control.Exception (bracket)
import System.Directory (removePathForcibly)

import Dumblog.Common.Metrics
import Dumblog.SQLite.DB
import Dumblog.SQLite.FrontEnd
import Dumblog.SQLite.Worker
import Journal.Internal.Metrics

------------------------------------------------------------------------

sqliteDumblog :: Int -> Int -> Maybe (MVar ()) -> IO ()
sqliteDumblog capacity port mReady = do
  queue <- newTBQueueIO (fromIntegral capacity)
  metrics <- newMetrics dumblogSchema (dumblogMetricsPath port)
  bracket initDB closeDB $ \conn ->
    withAsync (worker queue metrics conn) $ \_async ->
      runFrontEnd queue metrics port mReady

main :: IO ()
main = do
  putStrLn "Starting Dumblog (SQLite)"
  removePathForcibly sQLITE_DB_PATH
  sqliteDumblog (64 * 1024) 8054 Nothing
