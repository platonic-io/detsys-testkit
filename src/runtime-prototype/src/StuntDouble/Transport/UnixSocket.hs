module StuntDouble.Transport.UnixSocket where

import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Lazy.Char8 as BSLC
import Control.Concurrent (forkFinally)
import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Exception
import qualified Control.Exception as E
import Control.Monad(forever)
import qualified Data.Aeson as Aeson
import System.Directory
import System.FilePath
import System.IO
import System.IO.Error
import System.Posix.Files
import System.Timeout
import Network.Socket
import qualified Network.Socket.ByteString.Lazy as Socket

import StuntDouble.Codec
import StuntDouble.Envelope
import StuntDouble.Message
import StuntDouble.Reference
import StuntDouble.Transport

------------------------------------------------------------------------

unixSocketTransport :: FilePath -> EventLoopName -> Codec -> IO (Transport IO)
unixSocketTransport fp name codec@(Codec encode _) = withSocketsDo $ do
  queue <- newTBQueueIO 128 -- XXX: when/how does this grow?
  let udsFP = fp </> getEventLoopName name <> ".sock"
  putStrLn $ "Listening on: " <> udsFP
  cleanUpUnixDomainSocket udsFP
  aServer <- async (runServer udsFP codec queue)
  -- maybe we need to block until server is up?
  return Transport { transportSend = \e ->
                       let
                         addr = address (envelopeReceiver e)
                         payload = encodeEnvelope codec e
                       in
                         transportSend' (fp </> addr <> ".sock") payload
                   , transportReceive = atomically (tryReadTBQueue queue)
                   , transportShutdown = do
                       cancel aServer
                       cleanUpUnixDomainSocket udsFP
                   }

uSocket = socket AF_UNIX Stream defaultProtocol

runServer :: FilePath -> Codec -> TBQueue Envelope -> IO ()
runServer fp codec@(Codec _ decode) queue = do
  E.bracket open close loop
  where
    open = E.bracketOnError uSocket close $ \s -> do
      setSocketOption s ReuseAddr 1
      withFdSocket s setCloseOnExecIfNeeded
      putStrLn $ "Binding socket for: " <> fp
      bind s (SockAddrUnix fp)
      listen s 1024
      return s
    loop s = forever $ E.bracketOnError (accept s) (close . fst) $ \ (conn, peer) -> do
      forkFinally (server conn) (const $ gracefulClose conn 5000)
    server conn = do
      msg <- Socket.getContents conn
      case decodeEnvelope codec msg of
        Left err -> error err
        Right envelope -> do
          atomically $ writeTBQueue queue envelope
          server conn

-- we should have open connections?
transportSend' :: FilePath -> BSL.ByteString -> IO ()
transportSend' addr payload = do
  withSocketsDo $ E.bracket open close client
  where
    client c = do
      Socket.sendAll c payload
    open = E.bracketOnError uSocket close $ \s -> do
      connect s (SockAddrUnix addr)
      return s

cleanUpUnixDomainSocket :: FilePath -> IO ()
cleanUpUnixDomainSocket fp =
  catchJust
    (\e -> if isDoesNotExistErrorType (ioeGetErrorType e)
           then Just ()
           else Nothing)
    (removeFile fp)
    return
