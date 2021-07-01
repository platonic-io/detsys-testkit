{-# LANGUAGE OverloadedStrings #-}

-- This module is heavily inspired by Tyler Neely's sled benchmark:
-- https://github.com/spacejam/sled/blob/main/benchmarks/stress2/src/main.rs

module Main where

import Control.Monad
import Control.Exception
import Control.Concurrent
import Control.Concurrent.Async
import qualified Data.Time.Clock as Clock
import Data.Atomics.Counter
import Data.IORef

import StuntDouble

------------------------------------------------------------------------

scheduler :: Message -> Actor
-- XXX: use http frontend?
-- scheduler (ClientRequest "write" cid) = Actor $ do
scheduler (InternalMessage "write") = Actor $ do
  s <- get
  let Integer i = getField "i" s
  p <- asyncIO (IOAppend (Index (fromInteger i)) (Value "blob"))
  put (setField "i" (Integer (succ i)) s)
  return (InternalMessage "ack")

------------------------------------------------------------------------

client :: EventLoop -> LocalRef -> AtomicCounter -> AtomicCounter -> IORef Bool
       -> IO ()
client el lref total errors shutdown = go
  where
    go :: IO ()
    go = do
      b <- readIORef shutdown
      if b then return ()
      else do
        eReply <- try (ainvoke el lref (InternalMessage "write"))
                    :: IO (Either SomeException Message)
        case eReply of
          -- XXX: log error for debugging purposes?
          Left  _err   -> incrCounter_ 1 errors
          Right _reply -> return ()
        incrCounter_ 1 total
        go

-- | "Resident set size (RSS) is the portion of memory occupied by a process
-- that is held in main memory (RAM)." --
-- https://en.wikipedia.org/wiki/Resident_set_size
rss :: IO Double
rss = do
  -- XXX: This will only work on linux.
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

reporter :: AtomicCounter -> AtomicCounter -> IORef Bool -> IO ()
reporter total errors shutdown = go 0
  where
    go :: Int -> IO ()
    go last = do
      b <- readIORef shutdown
      if b then return ()
      else do
        threadDelay 1000000 -- 1s
        tot <- readCounter total
        err <- readCounter errors
        b <- rss
        putStrLn (concat ["did ", show (tot - last), " ops, ",
                          show (b / (1024 * 1024)), "mb RSS"])
        -- XXX: last for errors also?
        when (err /= 0) $
          putStrLn (show err ++ " errors")
        go tot

before :: Int -> AtomicCounter -> AtomicCounter -> IORef Bool
       -> IO (EventLoop, LocalRef, [Async ()])
before numberOfClients total errors shutdown = do
  el <- makeEventLoopThreaded SingleThreaded NoThreadPool
          realTime (makeSeed 0) (Http 3003) (EventLoopName "bench")
  lref <- spawn el scheduler (stateFromList [("i", Integer 0)])
  workerPids <- forM [0..numberOfClients] $ \i -> do
    async ((if i == 0 then reporter else client el lref) total errors shutdown)
  return (el, lref, workerPids)

data StoppingCriteria
  = MaxDurationInSecs Int
  | MaxOperations Int
  | WaitForCtrlCSignal

run :: AtomicCounter -> StoppingCriteria -> IORef Bool -> IO ()
run _total (MaxDurationInSecs s)  shutdown = do
  threadDelay (s * 1000000)
  writeIORef shutdown True
run _total WaitForCtrlCSignal     shutdown =
  threadDelay maxBound `finally` writeIORef shutdown True
run  total (MaxOperations maxOps) shutdown = go
  where
    go :: IO ()
    go = do
      c <- readCounter total
      if c < maxOps
      then do
        threadDelay (50 * 1000) -- 50 ms
        go
      else writeIORef shutdown True

after :: AtomicCounter -> AtomicCounter -> Clock.UTCTime
      -> (EventLoop, LocalRef, [Async ()]) -> IO ()
after total errors t0 (el, _lref, pids) = do
  now <- Clock.getCurrentTime
  let duration = Clock.diffUTCTime now t0
  quit el
  mapM_ wait pids
  tot <- readCounter total
  err <- readCounter errors
  putStrLn ""
  putStrLn (concat ["total of ", show tot, " ops in ", show duration,
                    " (", show (round (realToFrac tot / realToFrac duration)),
                    " ops/s)"])
  when (err /= 0) $
    putStrLn ("total errors: " ++ show err)

main :: IO ()
main = do
  n <- getNumCapabilities
  putStrLn ("CPU capabilities: " ++ show n)
  -- XXX: make it possilbe to configure these parameters via command line
  -- arguments:
  let numberOfClients = 4
      stop = MaxDurationInSecs 10

  total    <- newCounter 0
  errors   <- newCounter 0
  shutdown <- newIORef False
  now      <- Clock.getCurrentTime
  bracket
    (before numberOfClients total errors shutdown)
    (after total errors now)
    (const (run total stop shutdown))
