{-# LANGUAGE OverloadedStrings #-}

module Lec03.Service where

import Control.Concurrent
import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Exception (bracket)
import Control.Monad (forM_)
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy.Char8 as BS8
import Data.IORef
import Data.String (fromString)
import Data.Text.Read (decimal)
import Database.SQLite.Simple
       ( Connection
       , Only(Only)
       , close
       , execute
       , execute_
       , lastInsertRowId
       , open
       , query
       )
import Network.HTTP.Types.Status
import Network.Wai
import Network.Wai.Handler.Warp
import System.Environment

import Lec03.Queue
import Lec03.QueueInterface
import Lec03.QueueTest (fakeDequeue, fakeEnqueue, newModel)

------------------------------------------------------------------------

mAX_QUEUE_SIZE :: Int
mAX_QUEUE_SIZE = 128

pORT :: Int
pORT = 8050

------------------------------------------------------------------------

realQueue :: Int -> IO (QueueI a)
realQueue size = do
  q <- newQueue size
  return QueueI
    { qiEnqueue = enqueue q
    , qiDequeue = dequeue q
    }

fakeQueue :: Int -> IO (QueueI a)
fakeQueue size = do
  ref <- newIORef (newModel size)
  return QueueI
    { qiEnqueue = \x -> atomicModifyIORef' ref (fakeEnqueue x)
    , qiDequeue =       atomicModifyIORef' ref fakeDequeue
    }

------------------------------------------------------------------------

main :: IO ()
main = do
  args  <- getArgs
  queue <- case args of
             ["--testing"] -> fakeQueue mAX_QUEUE_SIZE
             _otherwise    -> realQueue mAX_QUEUE_SIZE
  service queue

service :: QueueI Command -> IO ()
service queue = do
  bracket initDB closeDB $ \conn ->
    withAsync (worker queue conn) $ \_a -> do
      _ready <- newEmptyMVar
      runFrontEnd queue _ready pORT

withService :: QueueI Command -> IO () -> IO ()
withService queue io = do
  bracket initDB closeDB $ \conn ->
    withAsync (worker queue conn) $ \_a -> do
      ready <- newEmptyMVar
      pid <- async (runFrontEnd queue ready pORT)
      confirm <- async (takeMVar ready)
      ok <- waitEither pid confirm
      case ok of
        Right () -> io
        Left  () -> error "Service should not return"

worker :: QueueI Command -> Connection -> IO ()
worker queue conn = go
  where
    go :: IO ()
    go = do
      mCmd <- qiDequeue queue
      case mCmd of
        Nothing -> do
          threadDelay 1000 -- 1 ms
          go
        Just cmd -> do
          exec cmd conn
          go

data Command
  = Write ByteString (MVar Int)
  | Read Int (MVar (Maybe ByteString))
  | Reset (MVar ()) -- For testing.

exec :: Command -> Connection -> IO ()
exec (Read ix response) conn = do
  bs <- readDB conn ix
  putMVar response bs
exec (Write bs response) conn = do
  ix <- writeDB conn bs
  putMVar response ix
exec (Reset response) conn = do
  resetDB conn
  putMVar response ()

httpFrontend :: QueueI Command -> Application
httpFrontend queue req respond =
  case requestMethod req of
    "GET" -> do
      case parseIndex of
        Nothing -> do
          respond (responseLBS status400 [] "Couldn't parse index")
        Just ix -> do
          response <- newEmptyMVar
          -- NOTE: We are not checking if the queue is full here...
          qiEnqueue queue (Read ix response)
          mbs <- takeMVar response
          case mbs of
            Nothing ->
              respond (responseLBS status404 [] (BS8.pack "Not found"))
            Just bs -> respond (responseLBS status200 [] bs)
    "POST" -> do
      bs <- consumeRequestBodyStrict req
      response <- newEmptyMVar
      qiEnqueue queue (Write bs response)
      ix <- takeMVar response
      respond (responseLBS status200 [] (BS8.pack (show ix)))
    "DELETE" -> do
      response <- newEmptyMVar
      qiEnqueue queue (Reset response)
      () <- takeMVar response
      respond (responseLBS status200 [] (BS8.pack "Reset"))
    _otherwise -> do
      respond (responseLBS status400 [] "Invalid method")
  where
    parseIndex :: Maybe Int
    parseIndex = case pathInfo req of
                   [txt] -> case decimal txt of
                     Right (ix, _rest) -> Just ix
                     _otherwise -> Nothing
                   _otherwise   -> Nothing

runFrontEnd :: QueueI Command -> MVar () -> Port -> IO ()
runFrontEnd queue ready port = runSettings settings (httpFrontend queue)
  where
    settings
      = setPort port
      $ setBeforeMainLoop (putMVar ready ())
      $ defaultSettings

------------------------------------------------------------------------

sQLITE_DB_PATH :: FilePath
sQLITE_DB_PATH = "/tmp/lec3_webservice.sqlite3"

sQLITE_FLAGS :: [String]
sQLITE_FLAGS = ["fullfsync=1", "journal_mode=WAL", "synchronous=NORMAL"]

sqlitePath :: String
sqlitePath =
  let
    flags = map (++ ";") sQLITE_FLAGS
  in
    sQLITE_DB_PATH ++ "?" ++ concat flags

initDB :: IO Connection
initDB = do
  conn <- open sqlitePath
  let flags = map (++ ";") sQLITE_FLAGS
  forM_ flags $ \flag -> do
    execute_ conn ("PRAGMA " <> fromString flag)
  execute_ conn "CREATE TABLE IF NOT EXISTS lec3_webservice (ix INTEGER PRIMARY KEY, value BLOB)"
  return conn

writeDB :: Connection -> ByteString -> IO Int
writeDB conn bs = do
  execute conn "INSERT INTO lec3_webservice (value) VALUES (?)" (Only bs)
  fromIntegral . pred <$> lastInsertRowId conn

readDB :: Connection -> Int -> IO (Maybe ByteString)
readDB conn ix = do
  result <- query conn "SELECT value from lec3_webservice WHERE ix = ?" (Only (ix + 1))
  case result of
    [[bs]]     -> return (Just bs)
    _otherwise -> return Nothing

resetDB :: Connection -> IO ()
resetDB conn = do
  execute_ conn "DROP TABLE IF EXISTS lec3_webservice"
  execute_ conn "CREATE TABLE IF NOT EXISTS lec3_webservice (ix INTEGER PRIMARY KEY, value BLOB)"

closeDB :: Connection -> IO ()
closeDB = close
