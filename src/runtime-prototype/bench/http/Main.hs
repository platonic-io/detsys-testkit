{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Concurrent
import Control.Concurrent.Async
import Control.Exception
import Control.Monad
import Data.Atomics.Counter
import Data.ByteString.Lazy (ByteString)
import Data.IORef
import Data.List (delete)
import qualified Data.Time.Clock as Clock
import Network.HTTP.Client
import Network.HTTP.Types.Status
import qualified Network.Wai as Wai
import qualified Network.Wai.Handler.Warp as Warp

------------------------------------------------------------------------

app :: Wai.Application
app _req respond = respond (Wai.responseLBS status200 [] "ack")

server :: Warp.Port -> IO (Async ())
server port = async (Warp.run port app)

makeClientRequest :: Request -> Manager -> IO ByteString
makeClientRequest request mgr = responseBody <$> httpLbs request mgr

prepareRequest :: ByteString -> Warp.Port -> IO Request
prepareRequest msg port = do
  let url :: String
      url = "http://localhost:" ++ show port

      body :: RequestBody
      body = RequestBodyLBS msg

  initialRequest <- parseRequest url

  return initialRequest
           { method      = "POST"
           , requestBody = body
           }

client :: Request -> Manager -> AtomicCounter -> AtomicCounter -> IORef Bool -> IO ()
client req mgr total errors shutdown = go
  where
    go :: IO ()
    go = do
      b <- readIORef shutdown
      if b then return ()
      else do
        reply <- makeClientRequest req mgr
        if reply == "ack"
        then return ()
        else incrCounter_ 1 errors
        incrCounter_ 1 total

        -- Doing async calls only seems to make things worse...

        -- as <- forM [1..5] $ \_ -> do
        --   async (makeClientRequest req mgr)

        -- let process [] = return ()
        --     process as = do
        --       (a, eReply) <- waitAnyCatch as
        --       case eReply of
        --         -- XXX: log error for debugging purposes?
        --         Left  _err  -> incrCounter_ 1 errors
        --         Right reply -> if reply == "ack"
        --                                then return ()
        --                                else incrCounter_ 1 errors
        --       incrCounter_ 1 total
        --       process (delete a as)

        -- process as
        go

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

before :: Int -> AtomicCounter -> AtomicCounter -> IORef Bool
       -> IO [Async ()]
before numberOfClients total errors shutdown = do
  serverPid <- server 3004
  req <- prepareRequest "write" 3004
  manager <- newManager defaultManagerSettings
                          -- 500 ms
                          { managerResponseTimeout =  responseTimeoutMicro (500 * 1000) }
  workerPids <- forM [0..numberOfClients] $ \i -> do
    async ((if i == 0 then reporter else client req manager) total errors shutdown)
  mapM_ link (serverPid : workerPids)
  return (serverPid : workerPids)

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

after :: AtomicCounter -> AtomicCounter -> Clock.UTCTime
      -> [Async ()] -> IO ()
after total errors t0 pids = do
  mapM_ wait (tail pids) -- workers
  cancel (head pids) -- http server
  printStats total errors t0

printStats :: AtomicCounter -> AtomicCounter -> Clock.UTCTime -> IO ()
printStats total errors t0 = do
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

main :: IO ()
main = do
  n <- getNumCapabilities
  putStrLn ("CPU capabilities: " ++ show n)
  let numberOfClients = 4
      stop = MaxDurationInSecs 30

  total    <- newCounter 0
  errors   <- newCounter 0
  shutdown <- newIORef False
  now      <- Clock.getCurrentTime
  bracket
    (before numberOfClients total errors shutdown)
    (after total errors now)
    (const (run total stop shutdown))
