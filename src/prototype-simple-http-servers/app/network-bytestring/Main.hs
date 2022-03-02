import Control.Monad (forever, void)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS
import qualified Control.Exception as E
import Network.Socket
import Network.Socket.ByteString (sendAll)
import Control.Concurrent

import TCPServer

main :: IO ()
main = withSocketsDo $ do
  n <- getNumCapabilities
  putStrLn ("Capabilities: " ++ show n)
  putStrLn "Starting http server on port 5002"
  sock <- listenOn 5002
  loop sock

loop :: Socket -> IO ()
loop sock = do
   (conn, _) <- accept sock
   forkIO $ body conn
   loop sock
  where
   body c = do sendAll c msg
               close c

msg :: ByteString
msg = BS.pack "HTTP/1.0 200 OK\r\nContent-Length: 5\r\n\r\nPong!\r\n"
