module Main where

import Control.Concurrent
import Control.Concurrent.Async
import Data.Atomics.Counter
import Data.IORef

import StuntDouble

------------------------------------------------------------------------

client :: AtomicCounter -> IORef Bool -> IO ()
client total shutdown = go
  where
    go :: IO ()
    go = do
      b <- readIORef shutdown
      if b then return ()
      else do
        -- generate client req
        -- execute client req
        incrCounter_ 1 total
        go

reporter :: IO ()
reporter = return ()

main :: IO ()
main = do
  -- spawn event loop
  -- deploy SUT
  let n = 4
      maxOps = 100
  total <- newCounter 0
  shutdown <- newIORef False
  pids <- mapM (\i -> async (if i == 0 then reporter else client total shutdown)) [0..n]
  go total maxOps shutdown
  mapM_ wait pids
  -- print stats
  where
    go :: AtomicCounter -> Int -> IORef Bool -> IO ()
    go total maxOps shutdown = go'
      where
        go' :: IO ()
        go' = do
          c <- readCounter total
          if c < maxOps
          then do
            threadDelay (50 * 1000) -- 50 ms
            go'
          -- else if maxDuration has elapsed, flip shutdown
          else do
            writeIORef shutdown True
