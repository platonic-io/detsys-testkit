{-# LANGUAGE OverloadedStrings #-}

module Dumblog.SQLite.DB
  (Connection, module Dumblog.SQLite.DB)
  where

import Data.ByteString.Lazy (ByteString)
import Database.SQLite.Simple
       ( Connection
       , Only(Only)
       , execute
       , execute_
       , lastInsertRowId
       , open
       , query
       , close
       )

------------------------------------------------------------------------

sQLITE_DB_PATH :: FilePath
sQLITE_DB_PATH = "/tmp/dumblog.sqlite3"

sQLITE_FLAGS :: String
sQLITE_FLAGS = "?fullfsync=true"

initDB :: IO Connection
initDB = do
  conn <- open (sQLITE_DB_PATH ++ sQLITE_FLAGS)
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
