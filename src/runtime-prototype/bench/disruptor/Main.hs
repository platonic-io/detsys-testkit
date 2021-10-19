module Main where

import Control.Concurrent
import Control.Concurrent.Async
import Data.Time
import Data.IORef
import Data.Int

import Disruptor

------------------------------------------------------------------------

iTERATIONS :: Int64
iTERATIONS = 1000 * 1000 * 100

main :: IO ()
main = do
  n <- getNumCapabilities
  putStrLn ("CPU capabilities: " ++ show n)
  let ringBufferCapacity = 1024 * 64
  rb <- newRingBuffer SingleProducer ringBufferCapacity
  transactions <- newIORef 0
  throughput   <- newIORef 0

  let production () = do
        atomicModifyIORef' transactions (\n -> (n + 1, ()))
        return (1 :: Int, ())
      backPressure () = return ()
  ep <- newEventProducer rb production backPressure ()
  let handler _s _n snr _endOfBatch = do
        t' <- atomicModifyIORef' transactions (\n -> let n' = n - 1 in (n', n'))
        n <- readIORef throughput
        let n' = n + 1
        writeIORef throughput n'
        -- print (realToFrac t' / realToFrac n' * 1000)
        return snr

  ec <- newEventConsumer rb handler 0 [] (Sleep 1)
  setGatingSequences rb [Exists ec]

  let areWeDoneConsuming = do
        t <- readIORef throughput
        if t >= fromIntegral iTERATIONS - 1
        then return ()
        else do
          threadDelay 10000
          areWeDoneConsuming
  start <- getCurrentTime
  withEventProducer ep $ \aep ->
    withEventConsumer ec $ \aec ->
     withAsync areWeDoneConsuming $ \a -> do
       wait a
       shutdownProducer ep
       wait aep
       shutdownConsumer ec
       events <- wait aec
       end <- getCurrentTime
       putStrLn ("Total number of events: " ++ show events)
       putStrLn ("Duration: " ++ show (realToFrac (diffUTCTime end start)) ++ " seconds")
       putStrLn ("Average: " ++
         show (realToFrac events / realToFrac (diffUTCTime end start)) ++ " events/s")
