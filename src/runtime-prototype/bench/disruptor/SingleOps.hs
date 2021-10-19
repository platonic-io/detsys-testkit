{-# LANGUAGE NumericUnderscores #-}

module Main where

import Control.Monad
import Data.Time
import Data.Word
import Data.Atomics.Counter
import Data.IORef
import System.CPUTime

import StuntDouble.Histogram

------------------------------------------------------------------------

main :: IO ()
main = do
  many "getCurrentTime" (return ()) (const getCurrentTime)

  many "getCPUTime" (return ()) (const getCPUTime)

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
  start <- fromInteger <$> getCPUTime
  _     <- io
  end   <- fromInteger <$> getCPUTime

  let diffPico :: Word64
      diffPico = end - start

      diffNano :: Double
      diffNano = realToFrac (fromIntegral diffPico) * 1e-3

  void (measure diffNano h)
