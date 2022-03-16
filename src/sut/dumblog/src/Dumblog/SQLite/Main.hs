module Dumblog.SQLite.Main where

import Control.Exception (bracket)
import Control.Concurrent (MVar)
import Control.Concurrent.Async (withAsync)
import Control.Concurrent.STM.TBQueue (newTBQueueIO)

import Journal.Internal.Metrics
import Dumblog.Common.Metrics
import Dumblog.SQLite.FrontEnd
import Dumblog.SQLite.Worker
import Dumblog.SQLite.DB

------------------------------------------------------------------------

sqliteDumblog :: Int -> Int -> Maybe (MVar ()) -> IO ()
sqliteDumblog capacity port mReady = do
  queue <- newTBQueueIO (fromIntegral capacity)
  metrics <- newMetrics dumblogSchema dUMBLOG_METRICS
  bracket initDB closeDB $ \conn ->
    withAsync (worker queue metrics conn) $ \_async ->
      runFrontEnd queue metrics port mReady

main :: IO ()
main = do
  putStrLn "Starting Dumblog (SQLite)"
  sqliteDumblog (64 * 1024) 8054 Nothing
