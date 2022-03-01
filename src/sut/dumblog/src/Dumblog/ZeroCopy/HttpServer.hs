module Dumblog.ZeroCopy.HttpServer where

import Data.Maybe (fromJust)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS8
import Control.Exception (bracketOnError)
import Network.Socket
import Network.Socket.ByteString (sendAll)
import GHC.Event
import Control.Concurrent

------------------------------------------------------------------------

httpServer :: Int -> IO ()
httpServer port = withSocketsDo $ do
  numCapabilities <- getNumCapabilities
  putStrLn ("Starting http server on port: " ++ show port)
  putStrLn ("Capabilities: : " ++ show numCapabilities)
  sock <- listenOn port
  mgr <- fromJust (error "Compile with -threaded") <$> getSystemEventManager
  _key <- withFdSocket sock $ \fd ->
    registerFd mgr (client sock) (fromIntegral fd) evtRead MultiShot
  loop
  where
    loop = do
      threadDelay (10*1000*1000)
      loop

client :: Socket -> FdKey -> Event -> IO ()
client sock _ _ = do
  (conn, _) <- accept sock
  sendAll conn msg
  close conn

msg :: ByteString
msg = BS8.pack "HTTP/1.0 200 OK\r\nContent-Length: 5\r\n\r\nPong!\r\n"


listenOn :: Int -> IO Socket
listenOn port = do
  addr <- resolve
  open addr
  where
    resolve = do
        let hints = defaultHints {
                addrFlags = [AI_PASSIVE]
              , addrSocketType = Stream
              }
        head <$> getAddrInfo (Just hints) (Just "localhost") (Just (show port))

    open addr = bracketOnError (openSocket addr) (const (return ())) $ \sock -> do
      -- close $ \sock -> do
      setSocketOption sock ReuseAddr 1
      withFdSocket sock setCloseOnExecIfNeeded
      bind sock (addrAddress addr)
      listen sock 1024
      return sock

openSocket :: AddrInfo -> IO Socket
openSocket addr = socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)
