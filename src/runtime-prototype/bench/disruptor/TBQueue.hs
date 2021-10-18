module Main where

import Control.Concurrent
import Control.Concurrent.Async
import Control.Concurrent.STM
import Data.IORef
import Data.Int
import Data.Time

------------------------------------------------------------------------

iTERATIONS :: Int64
iTERATIONS = 1000 * 1000 * 100

main :: IO ()
main = do
  n <- getNumCapabilities
  putStrLn ("CPU capabilities: " ++ show n)
  queue <- newTBQueueIO (1024 * 64)

  let producer n | n == iTERATIONS = return ()
                 | otherwise       = do
        atomically (writeTBQueue queue n)
        producer (n + 1)

      consumer = do
        n <- atomically (readTBQueue queue)
        if n == iTERATIONS - 1
        then return ()
        else consumer

  start <- getCurrentTime
  withAsync (producer 0) $ \ap ->
    withAsync consumer $ \ac -> do
       wait ap
       wait ac
       end <- getCurrentTime
       putStrLn ("Total number of events: " ++ show iTERATIONS)
       putStrLn ("Duration: " ++ show (realToFrac (diffUTCTime end start)) ++ " seconds")
       putStrLn ("Average: " ++
         show (realToFrac iTERATIONS / realToFrac (diffUTCTime end start)) ++ " events/s")
