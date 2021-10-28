{-# language NumericUnderscores#-}
module Main where

import Control.Concurrent
import Control.Concurrent.Async
import Data.IORef (IORef, newIORef, readIORef, writeIORef)

limit = 100_000_000

looper :: IORef Int -> IO ()
looper ref = do
  tid <- myThreadId
  putStr "looper: "
  print =<< threadCapability tid
  go
  where
    go = do
      r <- readIORef ref
      if r > limit then return () else go

other :: IORef Int -> IO ()
other ref = do
  tid <- myThreadId
  putStr "other: "
  print =<< threadCapability tid
  go
  where
    go = do
      r <- readIORef ref
      putStrLn $ "other running at " <> show r <> " out of " <> show limit
      writeIORef ref (r+1)
      if r > limit then return () else go

main :: IO ()
main = do
  n <- getNumCapabilities
  print n
  ref <- newIORef 0
  withAsyncOn 1 (looper ref) $ \ l ->
    withAsyncOn 4 (other ref) $ \ o -> do
      wait o
      wait l
