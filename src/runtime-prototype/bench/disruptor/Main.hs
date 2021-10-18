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

  let production   () = return (1 :: Int, ())
      backPressure () = return ()
  ep <- newEventProducer rb production backPressure ()
  let handler _s _n snr _endOfBatch = return (getSequenceNumber snr)
  ec <- newEventConsumer rb handler 0 [] (Sleep 1)
  setGatingSequences rb [Exists ec]

  let areWeDoneConsuming = do
        snr <- readIORef (ecSequenceNumber ec)
        -- NOTE: We need -1 below because the sequence number starts at 0.
        if snr >= fromIntegral iTERATIONS - 1
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
