{-# LANGUAGE NumericUnderscores#-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}

module Main where

import Control.Concurrent
import GHC.Exts
import GHC.Types

data AtomicCounter = AtomicCounter !(MutableByteArray# RealWorld)

newCounter :: Int -> IO AtomicCounter
newCounter (I# n) = IO $ \s ->
  case newAlignedPinnedByteArray# size alignment s of
    (# s', arr #) -> case writeIntArray# arr 0# n s' of
      s'' -> (# s'', AtomicCounter arr #)
  where
    !(I# size)      = 64
    !(I# alignment) = 64
{-# INLINE newCounter #-}

incrCounter :: Int -> AtomicCounter -> IO Int
incrCounter (I# incr) (AtomicCounter arr) = IO $ \s ->
  case fetchAddIntArray# arr 0# incr s of
    (# s', i #) -> (# s', I# (i +# incr) #)
{-# INLINE incrCounter #-}

readCounter :: AtomicCounter -> IO Int
readCounter (AtomicCounter arr) = IO $ \s ->
  case readIntArray# arr 0# s of
    (# s', i #) -> (# s', I# i #)
{-# INLINE readCounter #-}

limit = 100_000_000

looper :: AtomicCounter -> IO ()
looper ref = do
  tid <- myThreadId
  putStr "looper: "
  print =<< threadCapability tid
  go
  where
    go = do
      r <- readCounter ref -- NOTE: Using readIORef here instead causes a stall.
      if r > limit then return () else go

other :: AtomicCounter -> IO ()
other ref = do
  tid <- myThreadId
  putStr "other: "
  print =<< threadCapability tid
  go
  where
    go = do
      r <- incrCounter 1 ref
      putStrLn $ "other running at " <> show r <> " out of " <> show limit
      if r > limit then return () else go

main :: IO ()
main = do
  n <- getNumCapabilities
  print n
  ref <- newCounter 0
  tid1 <- forkOn 2 (looper ref)
  tid2 <- forkOn 4 (other ref)
  threadDelay 100000000
  putStrLn "time's up"
