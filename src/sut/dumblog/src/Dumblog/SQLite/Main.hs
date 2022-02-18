module Dumblog.SQLite.Main where

import Control.Exception (bracket)
import Control.Concurrent.Async (withAsync)
import Control.Concurrent.STM.TBQueue (newTBQueueIO)

import Dumblog.SQLite.FrontEnd
import Dumblog.SQLite.Worker
import Dumblog.SQLite.DB

------------------------------------------------------------------------

main :: IO ()
main = do
  queue <- newTBQueueIO (64*1024)
  bracket initDB closeDB $ \conn ->
    withAsync (worker queue conn) $ \_async ->
      runFrontEnd queue 8054
