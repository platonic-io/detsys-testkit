{-# LANGUAGE NumericUnderscores #-}

module Main where

import Control.Monad
import Control.Concurrent.Async
import Data.Time
import Data.Word
import Data.Atomics.Counter
import Data.IORef
import System.CPUTime

import StuntDouble.Histogram
import qualified StuntDouble.AtomicCounterPadded as Padded

------------------------------------------------------------------------

main :: IO ()
main = do
  many "getCurrentTime" (return ()) (const getCurrentTime)

  many "getCPUTime" (return ()) (const getCPUTime)

  many "incrCounter1" (newCounter 0) (incrCounter 1)
  many "incrCounter1Padded" (Padded.newCounter 0) (Padded.incrCounter 1)

  manyConcurrent "incrCounter1Concurrent"
    3 (newCounter 0) (incrCounter 1)
  manyConcurrent "incrCounter1PaddedConcurrent"
    3 (Padded.newCounter 0) (Padded.incrCounter 1)

  many "modifyIORef'" (newIORef (0 :: Int)) (\r -> modifyIORef' r succ)

  many "atomicModifyIORef'"
    (newIORef (0 :: Int)) (\r -> atomicModifyIORef' r (\n -> ((n + 1), ())))

  manyConcurrent "atomicModifyIORef'Concurrent" 3
    (newIORef (0 :: Int)) (\r -> atomicModifyIORef' r (\n -> ((n + 1), ())))

many :: String -> IO a -> (a -> IO b) -> IO ()
many name create use = do
  h <- newHistogram
  r <- create
  replicateM 500000 (once h (use r))
  putStrLn ""
  putStrLn ""
  prettyPrintHistogram name h

manyConcurrent :: String -> Int -> IO a -> (a -> IO b) -> IO ()
manyConcurrent name n create use = do
  h <- newHistogram
  r <- create
  as <- replicateM n (async (replicateM 500000 (once h (use r))))
  mapM_ wait as
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
