{-# language NumericUnderscores#-}
module Main where

import Control.Concurrent.Async
import Data.IORef (IORef, newIORef, readIORef, writeIORef)

limit = 100_000_000

looper :: IORef Int -> IO ()
looper ref = go
  where
    go = do
      r <- readIORef ref
      if r > limit then return () else go

other :: IORef Int -> IO ()
other ref = go
  where
    go = do
      r <- readIORef ref
      putStrLn $ "other running at " <> show r <> " out of " <> show limit
      writeIORef ref (r+1)
      if r > limit then return () else go

main :: IO ()
main = do
  ref <- newIORef 0
  withAsync (looper ref) $ \ l ->
    withAsync (other ref) $ \ o -> do
      wait o
      wait l
