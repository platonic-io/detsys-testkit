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

initDB :: IO Connection
initDB = do
  conn <- open sQLITE_DB_PATH
  execute_ conn "CREATE TABLE IF NOT EXISTS dumblog (ix INTEGER PRIMARY KEY, value BLOB)"
  return conn

writeDB :: Connection -> ByteString -> IO Int
writeDB conn bs = do
  execute conn "INSERT INTO dumblog (value) VALUES (?)" (Only bs)
  fromIntegral <$> lastInsertRowId conn

readDB :: Connection -> Int -> IO ByteString
readDB conn ix = do
  [[bs]] <- query conn "SELECT value from dumblog WHERE ix = ?" (Only ix)
  return bs

closeDB :: Connection -> IO ()
closeDB = close
