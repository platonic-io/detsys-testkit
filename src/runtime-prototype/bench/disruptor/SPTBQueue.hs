module Main where

import Control.Monad
import Control.Concurrent.STM
import Control.Concurrent.MVar

import Common

------------------------------------------------------------------------

main :: IO ()
main = spsc setup producer consumer
  where
    setup = newTBQueueIO (1024 * 64)

    producer q = replicateM_ iTERATIONS (atomically (writeTBQueue q 1))

    consumer q consumerFinished = do
      replicateM_ iTERATIONS (atomically (readTBQueue q))
      putMVar consumerFinished ()
