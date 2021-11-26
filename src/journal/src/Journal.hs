module Journal
  ( module Journal.Types
  , defaultOptions
  , startNewJournal
  , restartOldJournal
  , journal
  , journalSocket
  , journalSocket_
  , truncateAfterSnapshot
  , replay
  , replay_
  ) where

import Data.ByteString (ByteString)
import Network.Socket (Socket)

import Journal.Types

------------------------------------------------------------------------

-- * Initialisation

defaultOptions :: Options
defaultOptions = Options

startNewJournal :: FilePath -> Options -> IO Journal
startNewJournal dir opts = undefined
  -- XXX: assert dir is a r/w directory
  -- XXX: assert max size

restartOldJournal :: FilePath -> IO Journal
restartOldJournal dir = undefined

------------------------------------------------------------------------

-- * Production

journal :: Journal -> ByteString -> IO ()
journal = undefined

journalSocket :: Journal -> Socket -> Int -> IO ByteString
journalSocket jour sock len = undefined

journalSocket_ :: Journal -> Socket -> Int -> IO Int
journalSocket_ jour sock len = do
  buf <- undefined -- getPtrToActive jour
  undefined
  -- rxBytes <- recvBuf sock buf len
  -- return rxBytes

------------------------------------------------------------------------

-- * Consumption

readJournal :: Journal -> Int -> IO ByteString
readJournal = undefined

------------------------------------------------------------------------

-- * Snapshots and replay

truncateAfterSnapshot :: Journal -> Position -> IO ()
truncateAfterSnapshot = undefined

replay :: Journal -> Position -> (ByteString -> IO a) -> IO [a]
replay = undefined

replay_ :: Journal -> Position -> (ByteString -> IO ()) -> IO ()
replay_ = undefined
