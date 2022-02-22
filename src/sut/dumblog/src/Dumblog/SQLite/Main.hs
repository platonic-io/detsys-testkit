module Dumblog.SQLite.Main where

import Control.Exception (bracket)
import Control.Concurrent (MVar)
import Control.Concurrent.Async (withAsync)
import Control.Concurrent.STM.TBQueue (newTBQueueIO)

import Dumblog.SQLite.FrontEnd
import Dumblog.SQLite.Worker
import Dumblog.SQLite.DB

------------------------------------------------------------------------

sqliteDumblog :: Int -> Int -> Maybe (MVar ()) -> IO ()
sqliteDumblog capacity port mReady = do
  queue <- newTBQueueIO (fromIntegral capacity)
  bracket initDB closeDB $ \conn ->
    withAsync (worker queue conn) $ \_async ->
      runFrontEnd queue port mReady

main :: IO ()
main = do
  putStrLn "Starting Dumblog (SQLite)"
  sqliteDumblog (64 * 1024) 8054 Nothing
