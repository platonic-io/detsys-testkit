module Dumblog.SQLite.Worker where

import Control.Concurrent.MVar (putMVar)
import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TBQueue (TBQueue, readTBQueue)

import Dumblog.SQLite.DB
import Dumblog.SQLite.Command

------------------------------------------------------------------------

worker :: TBQueue Command -> Connection -> IO ()
worker queue conn = go
  where
    go :: IO ()
    go = do
      cmd <- atomically (readTBQueue queue)
      case cmd of
        Read ix response -> do
          bs <- readDB conn ix
          putMVar response bs
        Write bs response -> do
          ix <- writeDB conn bs
          putMVar response ix
      go
