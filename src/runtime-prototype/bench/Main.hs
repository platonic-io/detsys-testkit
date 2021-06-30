-- This module is heavily inspired by Tyler Neely's sled benchmark:
-- https://github.com/spacejam/sled/blob/main/benchmarks/stress2/src/main.rs

module Main where

import Control.Monad
import Control.Exception
import Control.Concurrent
import Control.Concurrent.Async
import Data.Atomics.Counter

import StuntDouble

------------------------------------------------------------------------

client :: AtomicCounter -> IO ()
client total = forever go
  where
    go :: IO ()
    go = do
      -- generate client req
      -- execute client req
      -- XXX: remove
      threadDelay 100000 -- 100ms
      incrCounter_ 1 total

rss :: IO Double
rss = do
  ml <- try (readFile "/proc/self/statm")
  case ml of
    Left err -> do
      print (err :: SomeException)
      return 0
    Right l   ->
      let
        rssPages = read (words l !! 1)
      in
        return (rssPages * 4096)

reporter :: AtomicCounter -> IO ()
reporter total = go 0
  where
    go :: Int -> IO ()
    go last = do
      threadDelay 1000000 -- 1s
      tot <- readCounter total
      b <- rss
      putStrLn (concat ["did ", show (tot - last), " ops, ",
                        show (b / (1024 * 1024)), "mb RSS"])
      go tot

before :: Int -> AtomicCounter -> IO [Async ()]
before numberOfClients total =
  mapM (\i -> async ((if i == 0 then reporter else client) total))
       [0..numberOfClients]

data StoppingCriteria
  = MaxDurationInSecs Int
  | MaxOperations Int
  | WaitForCtrlCSignal

run :: AtomicCounter -> StoppingCriteria -> IO ()
run _total (MaxDurationInSecs s)  = threadDelay (s * 1000000)
run  total (MaxOperations maxOps) = go
  where
    go :: IO ()
    go = do
      c <- readCounter total
      if c < maxOps
      then do
        threadDelay (50 * 1000) -- 50 ms
        go
      else return ()
run _total WaitForCtrlCSignal = threadDelay maxBound

after :: AtomicCounter -> [Async ()] -> IO ()
after total pids = do
  mapM_ cancel pids
  tot <- readCounter total
  putStrLn ""
  putStrLn ("total ops: " ++ show tot ++ " ops")

main :: IO ()
main = do
  -- spawn event loop
  -- deploy SUT
  let numberOfClients = 4
      stop = MaxOperations 100
  total <- newCounter 0
  bracket
    (before numberOfClients total)
    (after total)
    (const (run total stop))
