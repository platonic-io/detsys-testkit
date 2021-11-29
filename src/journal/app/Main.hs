module Main where

import Control.Concurrent.Async (withAsync)
import Control.Concurrent (forkFinally)
import qualified Control.Exception as E
import Control.Monad (unless, forever, void)
import Data.ByteString (ByteString)
import Data.ByteString.Internal (w2c)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSChar8
import Network.Socket
import Network.Socket.ByteString (sendAll, recv)
import Data.Word
import Foreign.Ptr

import Journal

------------------------------------------------------------------------

main :: IO ()
main = do
  (jour, jc) <- startJournal "/tmp/journal" defaultOptions
  putStrLn "Starting TCP server on port 3000"
  withAsync (runProducer jour) $ \_a ->
    runConsumer jc

runProducer :: Journal -> IO ()
runProducer jour = runTCPServer Nothing "3000" (go jour)
  where
    go :: Journal -> Socket -> IO ()
    go jour sock = do
      putStrLn "A client connected..."
      rxBs <- tee jour sock 5
      putStrLn ("Received: `" ++ BSChar8.unpack rxBs ++ "'")
      if BS.null rxBs
      then return ()
      else
        sendAll sock (BSChar8.pack ("Appended " ++ show (BS.length rxBs) ++ " bytes\n"))

runConsumer :: JournalConsumer -> IO ()
runConsumer jc = go 10
  where
    go 0 = return ()
    go n = do
      bs <- readJournal jc
      putStrLn ("Consumed: `" ++ BSChar8.unpack bs ++ "'")
      go (n - BS.length bs)

------------------------------------------------------------------------

-- Taken from the network package's documentation.
runTCPServer :: Maybe HostName -> ServiceName -> (Socket -> IO a) -> IO a
runTCPServer mhost port server = withSocketsDo $ do
    addr <- resolve
    E.bracket (open addr) close loop
  where
    resolve = do
        let hints = defaultHints {
                addrFlags = [AI_PASSIVE]
              , addrSocketType = Stream
              }
        head <$> getAddrInfo (Just hints) mhost (Just port)
    open addr = E.bracketOnError (openSocket addr) close $ \sock -> do
        setSocketOption sock ReuseAddr 1
        withFdSocket sock setCloseOnExecIfNeeded
        bind sock $ addrAddress addr
        listen sock 1024
        return sock

    openSocket :: AddrInfo -> IO Socket
    openSocket addr = socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)

    loop sock = forever $ E.bracketOnError (accept sock) (close . fst)
        $ \(conn, _peer) -> void $
            -- 'forkFinally' alone is unlikely to fail thus leaking @conn@,
            -- but 'E.bracketOnError' above will be necessary if some
            -- non-atomic setups (e.g. spawning a subprocess to handle
            -- @conn@) before proper cleanup of @conn@ is your case
            forkFinally (server conn) (const $ gracefulClose conn 5000)