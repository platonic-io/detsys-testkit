module Dumblog.SQLite.Worker where

import Control.Concurrent.MVar (putMVar)
import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TBQueue
       (TBQueue, flushTBQueue, readTBQueue)

import Dumblog.SQLite.Command
import Dumblog.SQLite.DB

------------------------------------------------------------------------

worker :: TBQueue Command -> Connection -> IO ()
worker queue conn = go
  where
    go :: IO ()
    go = do
      cmd <- atomically (readTBQueue queue)
      execute conn cmd
      go

batchingWorker :: TBQueue Command -> Connection -> IO ()
batchingWorker queue conn = go
  where
    go :: IO ()
    go = do
      cmds <- atomically (flushTBQueue queue)
      mapM_ (execute conn) cmds
      go

execute :: Connection -> Command -> IO ()
execute conn (Read ix response) = do
  bs <- readDB conn ix
  putMVar response bs
execute conn (Write bs response) = do
  ix <- writeDB conn bs
  putMVar response ix
