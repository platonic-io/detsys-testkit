module Main where

import Control.Concurrent (threadDelay)
import Control.Concurrent.MVar
import Control.Monad (replicateM_)

import Control.Concurrent.Chan.Unagi.NoBlocking.Unboxed

import Common

------------------------------------------------------------------------

main :: IO ()
main = spsc newChan (producer . fst) (\(_i, o) -> consumer o)
  where
    producer i = replicateM_ iTERATIONS (writeChan i (1 :: Int))

    consumer o consumerFinished = do
      replicateM_ iTERATIONS (readChan (threadDelay 1) o)
      putMVar consumerFinished ()
