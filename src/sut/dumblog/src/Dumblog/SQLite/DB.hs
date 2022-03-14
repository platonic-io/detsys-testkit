{-# LANGUAGE OverloadedStrings #-}

module Dumblog.SQLite.DB
  (Connection, module Dumblog.SQLite.DB)
  where

import Control.Monad (forM_)
import Data.ByteString.Lazy (ByteString)
import Data.String (fromString)
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

------------------------------------------------------------------------

sQLITE_DB_PATH :: FilePath
sQLITE_DB_PATH = "/tmp/dumblog.sqlite3"

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
  execute_ conn "CREATE TABLE IF NOT EXISTS dumblog (ix INTEGER PRIMARY KEY, value BLOB)"
  return conn

writeDB :: Connection -> ByteString -> IO Int
writeDB conn bs = do
  execute conn "INSERT INTO dumblog (value) VALUES (?)" (Only bs)
  fromIntegral <$> lastInsertRowId conn

readDB :: Connection -> Int -> IO (Maybe ByteString)
readDB conn ix = do
  result <- query conn "SELECT value from dumblog WHERE ix = ?" (Only ix)
  case result of
    [[bs]]     -> return (Just bs)
    _otherwise -> return Nothing

closeDB :: Connection -> IO ()
closeDB = close
