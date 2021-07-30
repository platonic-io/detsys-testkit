{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Applicative
import Control.Concurrent
import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Concurrent.STM.TSem
import Control.Exception
import Control.Monad
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Except
import Data.Atomics.Counter
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as BS
import Data.List (delete)
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Time.Clock as Clock
import GHC.Event
import GHC.Int
import Network.HTTP.Client
import Network.HTTP.Types.Status
import Network.Socket
import Network.Socket.ByteString.Lazy
import qualified Network.Wai as Wai
import qualified Network.Wai.Handler.Warp as Warp
import qualified System.Posix.IO as Posix
import System.Posix.Types
import System.Timeout

------------------------------------------------------------------------

app :: Wai.Application
app _req respond = respond (Wai.responseLBS status200 [] "ack")

server :: Warp.Port -> IO (Async ())
server port = do
  readyTMVar <- newEmptyTMVarIO
  let settings = Warp.setPort port
               . Warp.setLogger logger
               . Warp.setBeforeMainLoop (atomically (putTMVar readyTMVar ()))
               $ Warp.defaultSettings
  aServer <- async (Warp.runSettings settings app)
  aReady  <- async (atomically (takeTMVar readyTMVar))
  ok <- waitEither aServer aReady
  case ok of
    Left () -> error "server: impossible, server should not return"
    Right () -> return aServer
  where
    logger req status mFileSize
      | status /= status200 = print (req, status, mFileSize)
      | otherwise           = return ()

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
type MaxOpenConnections = Integer

type Shutdown = TMVar ()

orGo :: Shutdown -> IO () -> IO ()
shutdown `orGo` k = do
  b <- atomically (isEmptyTMVar shutdown)
  if b
  then k
  else putStrLn "shutting down"

client :: ExecutionMode -> Request -> Manager -> AtomicCounter -> AtomicCounter -> Shutdown
       -> IO ()
client Synchronous req mgr total errors shutdown = go
  where
    go :: IO ()
    go = shutdown `orGo` do
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
      shutdown `orGo` do
        a' <- async (makeClientRequest req mgr)
        go (a' : delete a as)
client (EventLoop maxOpenConn) req mgr total errors shutdown = do
  Just emgr <- getSystemEventManager
  tmgr <- getSystemTimerManager
  sem <- atomically (newTSem maxOpenConn)
  withSocketsDo $ do
    addr <- resolve
    let go seenFds = do
          -- putStrLn "waiting for semaphor or shutdown"
          e <- atomically (fmap Left (readTMVar shutdown) <|> fmap Right (waitTSem sem))
          case e of
            Left () -> putStrLn "shutting down client"
            Right () -> do
              -- putStrLn "got semaphor"
              eSock <- runExceptT (connectSocket emgr addr)
              case eSock of
                Left err -> do
                  incrCounter_ 1 errors
                  incrCounter_ 1 total
                  printError "connectSocket" err
                  atomically (signalTSem sem)
                  go seenFds
                Right _sock -> do
                  incrCounter_ 1 total
                  atomically (signalTSem sem)
                  go seenFds
    go Set.empty

connectSocket :: EventManager -> AddrInfo -> Result Socket
connectSocket emgr addr = do
  sock <- safeOpenSocket addr
  _ <- safeConnect sock addr

  -- NOTE: `connect` waits for the socket to be writable. Ideally we want to
  -- register a callback for when it's writeable so that we don't block here.

  -- safeRegisterFd emgr (onWrite emgr addr) sock evtWrite OneShot

  -- safeRegisterFd socket connected
  safeCloseSocket sock
  return sock
  {-
  `handleErrors` (\e -> case errorCtx e of
                     OpenSocket -> return Nothing
                     Connect    -> close sock >> return Nothing
                     RegisterFd -> close sock >> return Nothing)
                     SendAll    -> reconnectSocket sock
-}

fdKeyToSocket :: FdKey -> Result Socket
fdKeyToSocket fdKey = safeIO FdKeyToSocket (mkSocket (unFd (keyFd fdKey)))
  where
    unFd (Fd cint) = cint

safeCloseSocket :: Socket -> Result ()
safeCloseSocket sock = safeIO Close (close sock)

closeFdKey :: FdKey -> Result ()
closeFdKey fdKey = do
  sock <- fdKeyToSocket fdKey
  safeCloseSocket sock

onWrite :: EventManager -> AddrInfo -> IOCallback
onWrite emgr addr fdKey evt = void $ runExceptT $ do
  safeUnregisterFd emgr fdKey
  closeFdKey fdKey

  -- -- XXX: use `send` and support partial writes?
  -- let msg = "GET / HTTP/1.1\r\n\r\n"
  -- runExceptT (safeIO Close (close sock)) `handleErrors` printError "onWrite"
  -- written <- safeSend sock msg `handleErrors` (\e -> printError "onWrite" e >> return (-1))
  -- print (written == BS.length msg)
  -- if written == BS.length msg
  -- then safeUnregisterFd emgr fdKey `handleErrors` printError "onWrite" >> close sock
  -- else reconnectSocket emgr addr fdKey `handleErrors` printError "onWrite: reconnectSocket"

reconnectSocket :: EventManager -> AddrInfo -> FdKey -> Result ()
reconnectSocket emgr addr fdKey = do
  safeUnregisterFd emgr fdKey
  sock <- fdKeyToSocket fdKey
  safeIO Close (close sock)
  _sock <- connectSocket emgr addr
  return ()

resolve :: IO AddrInfo
resolve = do
  let hints = defaultHints { addrFlags = [AI_NUMERICHOST], addrSocketType = Stream }
      host  = "127.0.0.1"
      port  = "3004"
  head <$> getAddrInfo (Just hints) (Just host) (Just port)

data Error = Timeout Context | IOError Context IOException
  deriving Show

type Result a = ExceptT Error IO a

data Context
  = OpenSocket
  | Connect
  | SendAll
  | Send
  | RegisterFd
  | UnregisterFd
  | Close
  | FdKeyToSocket
  deriving Show

safeIO :: Context -> IO a -> Result a
safeIO ctx io = do
  -- liftIO (putStrLn (show ctx))
  mex <- liftIO (timeout 10000 (try io))
  case mex of
    Nothing         -> throwE (Timeout ctx)
    Just (Left err) -> throwE (IOError ctx err)
    Just (Right x)  -> return x

safeOpenSocket :: AddrInfo -> Result Socket
safeOpenSocket addr = safeIO OpenSocket $ do
  sock <- socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)
  withFdSocket sock setNonBlockIfNeeded
  return sock

safeConnect :: Socket -> AddrInfo -> Result ()
safeConnect sock addr = safeIO Connect $ do
  connect sock (addrAddress addr)
  setSocketOption sock NoDelay 1

safeRegisterFd :: EventManager -> IOCallback -> Socket -> Event -> Lifetime
               -> Result FdKey
safeRegisterFd emgr cb sock evt lt =
  safeIO RegisterFd $ withFdSocket sock $ \fd ->
    registerFd emgr cb (Fd fd) evt lt

safeUnregisterFd :: EventManager -> FdKey -> Result ()
safeUnregisterFd emgr fdKey =
  safeIO UnregisterFd (unregisterFd emgr fdKey)

safeSendAll :: Socket -> ByteString -> Result ()
safeSendAll sock msg = safeIO SendAll (sendAll sock msg)

safeSend :: Socket -> ByteString -> Result Int64
safeSend sock msg = safeIO Send (send sock msg)

handleErrors :: IO (Either Error a) -> (Error -> IO a) -> IO a
handleErrors io k = do
  ex <- io
  case ex of
    Left err -> k err
    Right x  -> return x

printError :: String -> Error -> IO ()
printError ctx err = do
  putStr (ctx ++ ": ")
  print err

printIOException :: String -> IOException -> IO ()
printIOException ctx err = do
  putStr (ctx ++ ": ")
  print err

reporter :: AtomicCounter -> AtomicCounter -> Shutdown -> IO ()
reporter total errors shutdown = go 0 0
  where
    go :: Int -> Int -> IO ()
    go last lastErrs = shutdown `orGo` do
      threadDelay 1000000 -- 1s
      tot <- readCounter total
      err <- readCounter errors
      b <- rss
      putStrLn (concat ["did ", show (tot - last), " ops, ",
                        show (b / (1024 * 1024)), "mb RSS"])
      let errorsSinceLastTime = err - lastErrs
      when (errorsSinceLastTime /= 0) $
        putStrLn (show errorsSinceLastTime ++ " errors")
      go tot err

-- | "Resident set size (RSS) is the portion of memory occupied by a process
-- that is held in main memory (RAM)." --
-- https://en.wikipedia.org/wiki/Resident_set_size
rss :: IO Double
rss = do
  -- XXX: This will only work on linux.
  ml <- try (readFile "/proc/self/statm")
  case ml of
    Left err -> do
      print (err :: IOException)
      return 0
    Right l   ->
      let
        rssPages = read (words l !! 1)
      in
        return (rssPages * 4096)

before :: Int -> ExecutionMode -> AtomicCounter -> AtomicCounter -> Shutdown
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

run :: AtomicCounter -> StoppingCriteria -> Shutdown -> IO ()
run _total (MaxDurationInSecs s)  shutdown =
  threadDelay (s * 1000000) `finally` atomically (putTMVar shutdown ())
run _total WaitForCtrlCSignal     shutdown =
  forever (threadDelay maxBound) `finally` atomically (putTMVar shutdown ())
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
      else atomically (putTMVar shutdown ())

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
  let numberOfClients = 1
      stop = MaxDurationInSecs 3
      exec = EventLoop 1

  total    <- newCounter 0
  errors   <- newCounter 0
  shutdown <- newEmptyTMVarIO
  now      <- Clock.getCurrentTime
  bracket
    (before numberOfClients exec total errors shutdown)
    (after total errors now)
    (const (run total stop shutdown))

{-

    {-
              withSafeOpenSocket addr signalAndGo $ \sock ->
                safeConnect sock closeSignalAndGo $ \() ->
                  safeSend sock closeSignalAndGo "GET /index.html HTTP/1.1\r\n\r\n" $ \() -> do
                    safeRegisterCallbacks sock closeSignalAndGo

-}
              bracketOnError (open addr sem)
                             (\sock -> close sock >> atomically (signalTSem sem)) $ \sock -> do
                me <- timeout 1000 (try (sendAll sock "GET /index.html HTTP/1.1\r\n\r\n"))
                case me of
                  Nothing -> do
                    putStrLn "send timeout"
                    putStrLn "going 1"
                    incrCounter_ 1 errors
                    go seenFds
                  Just (Left err) -> do
                    printIOException "send error" err
                    close sock
                    atomically (signalTSem sem)
                    incrCounter_ 1 errors
                    putStrLn "going 2"
                    go seenFds
                  Just (Right ()) -> withFdSocket sock $ \fd -> do
                    putStrLn "   sent"
                    eFdKey <- try (registerFd emgr (onRead emgr sem) (Fd fd) evtRead OneShot)
                    putStrLn "     registered read callback"
                    case eFdKey of
                      Left err -> do
                        printIOException "client: registerFd" err
                        putStr "current fd: "
                        print fd
                        putStr "seenFds: "
                        print seenFds
                        incrCounter_ 1 errors
                        -- close sock
                        putStrLn "   closed"
                        atomically (signalTSem sem)
                        putStrLn "going 3"
                        go (Set.insert fd seenFds)
                      Right fdKey -> do
                        let timeoutCallback = do
                              eBool <- try (unregisterFd_ emgr fdKey)
                              case eBool of
                                Left err -> printIOException "timeout callback" err
                                Right _bool -> return ()
                              -- closeFd emgr Posix.closeFd (keyFd fdKey)
                              --   `catch` printIOException "closeFd"
                              atomically (signalTSem sem)
                              putStrLn "timed out"
                        -- _toKey <- registerTimeout tmgr 500000 timeoutCallback
                        -- putStrLn "     registered timeout callback"
                        putStrLn "going 4"
                        go (Set.insert fd seenFds)

            Right () -> putStrLn "shutting down"
    go Set.empty
  where
    open :: AddrInfo -> TSem -> IO Socket
    open addr sem = bracketOnError (openSocket addr)
                      (\sock -> close sock >> atomically (signalTSem sem)) $ \sock -> do
      putStrLn "opened"
      meu <- timeout 10000 $ try (connect sock (addrAddress addr))
      case meu of
        Nothing -> do
          -- close sock
          putStrLn "   closed"
          incrCounter_ 1 errors
          atomically (signalTSem sem)
        Just (Left e) -> do
          printIOException "open" e
          -- close sock
          putStrLn "   closed"
          incrCounter_ 1 errors
          atomically (signalTSem sem)
        Just (Right ()) -> putStrLn " connected"
      return sock

    onRead :: EventManager -> TSem -> FdKey -> Event -> IO ()
    onRead emgr sem fdKey evt
      | evt /= evtRead = do
          putStrLn ("onRead: " ++ show evt)
          incrCounter_ 1 errors
          atomically (signalTSem sem)
          putStrLn "       closed"
      | evt == evtRead = do
          go' `catch` \e -> do
            printIOException "onRead" e
            incrCounter_ 1 errors
          incrCounter_ 1 total
          eBool <- try (unregisterFd_ emgr fdKey)
          case eBool of
            Left err -> printIOException "onRead: unregisterFd_" err
            Right _bool -> return ()
          closeFd emgr Posix.closeFd (keyFd fdKey)
            `catch` printIOException "closeFd"
          putStrLn "       closed"
          atomically (signalTSem sem)

      where
        go' :: IO ()
        go' = do
          let Fd cint = keyFd fdKey
          sock <- mkSocket cint
          meReply <- timeout 1000 (try (recv sock 1024))
          case meReply of
            Nothing -> putStrLn "nothing to read"
            Just (Left err) -> do
              printIOException "onRead: go'" err
              incrCounter_ 1 errors
            Just (Right reply) -> do
              -- XXX: better parsing of response body
              if BS.split 10 reply !! 6 == "ack\r"
              then putStrLn "      read"
              else incrCounter_ 1 errors
openSocket :: AddrInfo -> IO Socket
openSocket addr = do
  sock <- socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)
  withFdSocket sock setNonBlockIfNeeded `catch` printIOException "openSocket"
  return sock

-}
