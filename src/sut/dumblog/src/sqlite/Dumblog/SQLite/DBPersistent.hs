{-# LANGUAGE OverloadedStrings #-}

-- NOTE: Using this module is slower than the `DB` module, probably
-- because `writeDB` does an extra `SELECT` to figure out the `ix` and
-- because we are converting to and from lazy `ByteString`s (because
-- that's what `http-client` uses). We could imagine keeping a counter
-- for `ix` inside `Connection` instead and if we had a http client that
-- uses strict `ByteString`s instead then perhaps this module would be
-- faster than `DB`.

module Dumblog.SQLite.DBPersistent
  (Connection, module Dumblog.SQLite.DBPersistent)
  where

import Data.ByteString.Lazy (ByteString, fromStrict, toStrict)
import Data.Text (Text)
import Database.Persist.PersistValue
import Database.Sqlite

------------------------------------------------------------------------

sQLITE_DB_PATH :: Text
sQLITE_DB_PATH = "file:///tmp/dumblog.sqlite3"

sQLITE_FLAGS :: Text
sQLITE_FLAGS = "?fullfsync=1;journal_mode=WAL;"

initDB :: IO Connection
initDB = do
  conn <- open (sQLITE_DB_PATH <> sQLITE_FLAGS)
  smt <- prepare conn
           "CREATE TABLE IF NOT EXISTS dumblog (ix INTEGER PRIMARY KEY, value BLOB);"
  _ <- stepConn conn smt
  return conn

writeDB :: Connection -> ByteString -> IO Int
writeDB conn bs = do
  stm <- prepare conn "INSERT INTO dumblog (value) VALUES (?);"
  bindBlob stm 1 (toStrict bs)
  _stepRes <- stepConn conn stm

  stm' <- prepare conn "SELECT MAX(ix) FROM dumblog;"
  rows <- stepConn conn stm' >> columns stm'
  case rows of
    [PersistInt64 ix] -> return (fromIntegral ix)
    _otherwise -> error "writeDB: impossible"

readDB :: Connection -> Int -> IO (Maybe ByteString)
readDB conn ix = do
  stm <- prepare conn "SELECT value from dumblog WHERE ix = ?;"
  bindInt stm 1 ix
  rows <- stepConn conn stm >> columns stm
  case rows of
    [PersistByteString bs] -> return (Just (fromStrict bs))
    _otherwise -> return Nothing

closeDB :: Connection -> IO ()
closeDB = close
