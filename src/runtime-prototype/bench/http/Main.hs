{-# LANGUAGE OverloadedStrings #-}

module Main where

import Network.Socket
import Network.Socket.ByteString.Lazy
import GHC.Event
import Control.Concurrent
import Control.Concurrent.Async
import Control.Exception
import Control.Monad
import Data.Atomics.Counter
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as BS
import System.Posix.Types
import System.Timeout
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
           { method      = "GET"
           -- , requestBody = body
           }

data ExecutionMode
  = Synchronous
  | Asynchronous BatchSize
  | EventLoop MaxOpenConnections

type BatchSize = Int
type MaxOpenConnections = Int

client :: ExecutionMode -> Request -> Manager -> AtomicCounter -> AtomicCounter -> IORef Bool
       -> IO ()
client Synchronous req mgr total errors shutdown = go
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
        go
client (Asynchronous batchSize) req mgr total errors shutdown = do
  as <- forM [1..batchSize] $ \_ -> do
    async (makeClientRequest req mgr)
  go as
  where
    go :: [Async ByteString] -> IO ()
    go [] = return ()
    go as = do
      (a, eReply) <- waitAnyCatch as
      case eReply of
        -- XXX: log error for debugging purposes?
        Left  _err  -> incrCounter_ 1 errors
        Right reply -> if reply == "ack"
                       then return ()
                       else incrCounter_ 1 errors

      incrCounter_ 1 total
      b <- readIORef shutdown
      if b then return ()
      else do
        a' <- async (makeClientRequest req mgr)
        go (a' : delete a as)
client (EventLoop maxOpenConn) req mgr total errors shutdown = do
  Just emgr <- getSystemEventManager
  openConnsCounter <- newCounter 0
  withSocketsDo $ do
    addr <- resolve
    let go = do
          b <- readIORef shutdown
          if b
          then putStrLn "stopping"
          else do
            currOpenConns <- readCounter openConnsCounter
            if currOpenConns < maxOpenConn
            then do
              bracketOnError (open addr) close $ \sock -> do
                incrCounter_ 1 openConnsCounter
                withFdSocket sock $ \fd -> do
                  eFdKey <- try (registerFd emgr (onRead emgr openConnsCounter)
                                            (Fd fd) evtRead OneShot)
                  case eFdKey of
                    Left err -> printSomeException "client: registerFd" err
                    Right _fdKey -> return ()
            else threadDelay 100
            go
    go
  where
    onRead :: EventManager -> AtomicCounter -> FdKey -> Event -> IO ()
    onRead emgr openConnsCounter fdKey evt
      | evt /= evtRead = return ()
      | evt == evtRead = go `catch` \e -> do
                           printSomeException "onRead" e
                           eBool <- try (unregisterFd_ emgr fdKey)
                           case eBool of
                             Left err -> printSomeException "onRead: unregisterFd_" err
                             Right _bool -> return ()

      where
        go :: IO ()
        go = do
          let Fd cint = keyFd fdKey
          sock <- mkSocket cint
          mReply <- timeout 1000 (recv sock 1024)
          case mReply of
            Nothing -> putStrLn "nothing to read"
            Just reply -> do
              -- XXX: better parsing of response body
              if BS.split 10 reply !! 6 == "ack\r"
              then return ()
              else incrCounter_ 1 errors
              incrCounter_ 1 total
              _ <- unregisterFd_ emgr fdKey
              incrCounter_ (-1) openConnsCounter
              close sock

    resolve = do
      let hints = defaultHints { addrFlags = [AI_NUMERICHOST], addrSocketType = Stream }
          host  = "127.0.0.1"
          port  = "3004"
      head <$> getAddrInfo (Just hints) (Just host) (Just port)

    open addr = bracketOnError (openSocket addr) close $ \sock -> do
      connect sock (addrAddress addr)
      _ <- timeout 1000 (sendAll sock "GET /index.html HTTP/1.1\r\n\r\n")
      return sock

    openSocket addr = socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)

printSomeException :: String -> SomeException -> IO ()
printSomeException ctx err = do
  putStr (ctx ++ ": ")
  print err

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

before :: Int -> ExecutionMode -> AtomicCounter -> AtomicCounter -> IORef Bool
       -> IO [Async ()]
before numberOfClients execution total errors shutdown = do
  serverPid <- server 3004
  req <- prepareRequest "write" 3004
  manager <- newManager defaultManagerSettings
                          -- 500 ms
                          { managerResponseTimeout =  responseTimeoutMicro (500 * 1000) }
  workerPids <- forM [0..numberOfClients] $ \i ->
    if i == 0
    then do
      pid <- async (reporter total errors shutdown)
      putStrLn "started reporter"
      return pid
    else do
      pid <- async (client execution req manager total errors shutdown)
      putStrLn ("started client " ++ show i)
      return pid
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
      stop = MaxDurationInSecs 5
      exec = EventLoop 1

  total    <- newCounter 0
  errors   <- newCounter 0
  shutdown <- newIORef False
  now      <- Clock.getCurrentTime
  bracket
    (before numberOfClients exec total errors shutdown)
    (after total errors now)
    (const (run total stop shutdown))
