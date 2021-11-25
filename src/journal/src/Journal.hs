module Journal
  ( module Journal.Types
  , defaultOptions
  , startNewJournal
  , restartOldJournal
  , journal
  , journalMany
  , truncateAfterSnapshot
  , replay
  , replay_
  ) where

import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS

-- XXX:
import Control.Concurrent (forkFinally)
import qualified Control.Exception as E
import Control.Monad (unless, forever, void)
import qualified Data.ByteString as S
import Network.Socket
import Network.Socket.ByteString (recv, sendAll)
import System.IO.MMap
import Data.Word
import Foreign.Ptr

import Journal.Types

------------------------------------------------------------------------

defaultOptions :: Options
defaultOptions = Options

startNewJournal :: FilePath -> Options -> IO Journal
startNewJournal dir opts = undefined
  -- XXX: assert dir is a r/w directory
  -- XXX: assert max size

restartOldJournal :: FilePath -> IO Journal
restartOldJournal dir = undefined

journal :: Journal -> ByteString -> IO Position
journal = undefined

journalMany :: Foldable t => Journal -> t ByteString -> IO Position
journalMany = undefined

journalSocket :: Journal -> Socket -> Int -> IO (Position, ByteString)
journalSocket jour sock len = undefined

truncateAfterSnapshot :: Journal -> Position -> IO ()
truncateAfterSnapshot = undefined

replay :: Journal -> Position -> (ByteString -> IO a) -> IO [a]
replay = undefined

replay_ :: Journal -> Position -> (ByteString -> IO ()) -> IO ()
replay_ = undefined

------------------------------------------------------------------------
-- * Prototype application integration

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

main :: IO ()
main = do
  mmapWithFilePtr "/tmp/mmap.txt" ReadWrite Nothing $ \(ptr, len) -> do
    putStrLn ("Memory mapped file length: " ++ show len)
    -- XXX: advance ptr past all written data using headers (len above is merely
    -- the size of the file not the contents).
    putStrLn "Listening on localhost:3000"
    runTCPServer Nothing "3000" (go (castPtr ptr))
  where
    go :: Ptr Word8 -> Socket -> IO ()
    go buf sock = do
      putStrLn "Waiting for client..."
      rx <- recvBuf sock buf 1024
      if rx == 0
      then return ()
      else do
        sendAll sock (BS.pack ("Appended " ++ show rx ++ " bytes\n"))
        go (buf `plusPtr` rx) sock
