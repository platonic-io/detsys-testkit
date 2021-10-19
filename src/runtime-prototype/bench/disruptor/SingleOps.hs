{-# LANGUAGE NumericUnderscores #-}

module Main where

import Control.Monad
import Data.Time
import Data.Atomics.Counter
import Data.IORef

import StuntDouble.Histogram

------------------------------------------------------------------------

main :: IO ()
main = do
  many "getCurrentTime" (return ()) (const getCurrentTime)

  many "incrCounter1" (newCounter 0) (incrCounter 1)

  many "modifyIORef'" (newIORef (0 :: Int)) (\r -> modifyIORef' r succ)

  many "atomicModifyIORef'"
    (newIORef (0 :: Int)) (\r -> atomicModifyIORef' r (\n -> ((n + 1), ())))

many :: String -> IO a -> (a -> IO b) -> IO ()
many name create use = do
  h <- newHistogram
  r <- create
  replicateM 500000 (once h (use r))
  putStrLn ""
  putStrLn ""
  prettyPrintHistogram name h

once :: Histogram -> IO a -> IO ()
once h io = do
  start <- getCurrentTime
  _     <- io
  end   <- getCurrentTime
  -- NOTE: diffUTCTime has a precision of 10^-12 s, so by multiplying by 10^9 we
  -- get milliseconds.
  void (measure (realToFrac (diffUTCTime end start) * 1_000_000_000) h)
