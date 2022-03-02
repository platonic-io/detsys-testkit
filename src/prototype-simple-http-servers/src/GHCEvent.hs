module GHCEvent where

import Control.Exception
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS
import Network.Socket
import Network.Socket.ByteString (sendAll)
import GHC.Event
import Control.Concurrent

import TCPServer

------------------------------------------------------------------------

main :: IO ()
main = withSocketsDo $ do
  n <- getNumCapabilities
  putStrLn ("Capabilities: " ++ show n)
  putStrLn "Starting http server on port 5002"
  sock <- listenOn 5002
  Just mgr <- getSystemEventManager
  key <- withFdSocket sock $ \fd ->
    registerFd mgr (client sock) (fromIntegral fd) evtRead MultiShot
  loop
  where
    loop = do
      threadDelay (10*1000*1000)
      loop

client :: Socket -> FdKey -> Event -> IO ()
client sock _ _ = do
    (c,_) <- accept sock
    sendAll c msg
    close c

msg :: ByteString
msg = BS.pack "HTTP/1.0 200 OK\r\nContent-Length: 5\r\n\r\nPong!\r\n"
