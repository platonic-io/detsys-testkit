{-# LANGUAGE OverloadedStrings #-}

-- This module is heavily inspired by Tyler Neely's sled benchmark:
-- https://github.com/spacejam/sled/blob/main/benchmarks/stress2/src/main.rs

module Main where

import Control.Concurrent
import Control.Concurrent.Async
import Control.Exception
import Control.Monad
import Data.List (delete)
import Data.Atomics.Counter
import Data.IORef
import qualified Data.Time.Clock as Clock
import Network.HTTP.Client

import StuntDouble

------------------------------------------------------------------------

  {-
scheduler :: Message -> Actor
scheduler (ClientRequest "write" cref) = Actor $ do
  s <- get
  let Integer i = getField "i" s
  p <- asyncIO (IOAppend (Index (fromInteger i)) (Value "blob"))
  put (setField "i" (Integer (succ i)) s)
  on p (const (clientResponse cref (InternalMessage "ack")))
  return (InternalMessage "dummyAck")

------------------------------------------------------------------------

client :: Manager -> EventLoop -> LocalRef -> AtomicCounter -> AtomicCounter -> IORef Bool
       -> IO ()
client mgr el lref total errors shutdown = go
  where
    go :: IO ()
    go = do
      b <- readIORef shutdown
      if b then return ()
      else do
        eReply <- makeClientRequest mgr (InternalMessage "write") 3004
        case eReply of
          -- XXX: log error for debugging purposes?
          Left  _err  -> incrCounter_ 1 errors
          Right reply -> if reply == InternalMessage "ack"
                         then return ()
                         else incrCounter_ 1 errors
        incrCounter_ 1 total

        -- as <- forM [1..5] $ \_ -> do
        --   async (makeClientRequest mgr (InternalMessage "write") 3004)

        -- let process [] = return ()
        --     process as = do
        --       (a, eReply) <- waitAnyCatch as
        --       case eReply of
        --         -- XXX: log error for debugging purposes?
        --         Left  _err          -> incrCounter_ 1 errors
        --         Right (Left _err)   -> incrCounter_ 1 errors
        --         Right (Right reply) -> if reply == InternalMessage "ack"
        --                                then return ()
        --                                else incrCounter_ 1 errors
        --       incrCounter_ 1 total
        --       process (delete a as)

        -- process as
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

before :: EventLoop -> LocalRef -> Int -> AtomicCounter -> AtomicCounter -> IORef Bool
       -> IO [Async ()]
before el lref numberOfClients total errors shutdown = do
  frontendPid <- startHttpFrontend el lref 3004
  manager <- newManager defaultManagerSettings
                          -- 500 ms
                          { managerResponseTimeout =  responseTimeoutMicro (500 * 1000) }
  workerPids <- forM [0..numberOfClients] $ \i -> do
    async ((if i == 0 then reporter else client manager el lref) total errors shutdown)
  mapM_ link (frontendPid : workerPids)
  return (frontendPid : workerPids)

data StoppingCriteria
  = MaxDurationInSecs Int
  | MaxOperations Int
  | WaitForCtrlCSignal

run :: AtomicCounter -> StoppingCriteria -> IORef Bool -> IO ()
run _total (MaxDurationInSecs s)  shutdown =
  threadDelay (s * 1000000) `finally` writeIORef shutdown True
run _total WaitForCtrlCSignal     shutdown =
  threadDelay maxBound `finally` writeIORef shutdown True
run  total (MaxOperations maxOps) shutdown =
  go
  where
    go :: IO ()
    go = do
      c <- readCounter total
      if c < maxOps
      then do
        threadDelay (50 * 1000) -- 50 ms
        go
      else writeIORef shutdown True

after :: EventLoop -> AtomicCounter -> AtomicCounter -> Clock.UTCTime
      -> [Async ()] -> IO ()
after el total errors t0 pids = do
  quit el
  mapM_ wait (tail pids) -- workers
  cancel (head pids) -- http frontend
  printStats el total errors t0
  prettyPrintHistogram "event loop saturation" (mEventLoopSat (lsMetrics el))

printStats :: EventLoop -> AtomicCounter -> AtomicCounter -> Clock.UTCTime -> IO ()
printStats ls total errors t0 = do
  now <- Clock.getCurrentTime
  let duration = Clock.diffUTCTime now t0
  tot <- readCounter total
  err <- readCounter errors
  putStrLn ""
  putStrLn (concat ["total of ", show tot, " ops in ", show duration,
                    " (", show (round (realToFrac tot / realToFrac duration)),
                    " ops/s)"])
  when (err /= 0) $
    putStrLn ("total errors: " ++ show err)

  serverSideErrs <- readCounter (mErrorCounter (lsMetrics ls))
  when (serverSideErrs /= 0) $
    putStrLn ("total server side errors: " ++ show serverSideErrs)

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
  el       <- makeEventLoopThreaded SingleThreaded NoThreadPool
                realTime (makeSeed 0) (Http 3003) (EventLoopName "bench")
  lref     <- spawn el scheduler (stateFromList [("i", Integer 0)])
  bracket
    (before el lref numberOfClients total errors shutdown)
    (after el total errors now)
    (const (run total stop shutdown))
    `finally` do
      printStats el total errors now

-}
