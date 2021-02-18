{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Ldfi.Storage where

import Control.Exception
import Data.List (groupBy)
import Database.SQLite.Simple
import Ldfi.Traces
import System.Environment
import System.FilePath
import System.IO.Error

------------------------------------------------------------------------

type TestId = Int

type RunId = Int

data LdfiEvent = LdfiEvent
  { leTestId :: TestId,
    leRunIds :: [RunId],
    leFaults :: [String], -- XXX: Fault?
    leVersion :: String,
    leStatistics :: String
  }

data Storage m = Storage
  { load :: TestId -> m [Trace],
    store :: LdfiEvent -> m ()
  }

mockStorage :: Monad m => [Trace] -> Storage m
mockStorage ts =
  Storage
    { load = const (return ts),
      store = const (return ())
    }

getDbPath :: IO String
getDbPath = do
  getEnv "DETSYS_DB"
    `catchIOError` \(e :: catchIOError) ->
      if isDoesNotExistError e
        then do
          home <- getEnv "HOME"
          return (home </> ".detsys.db")
        else throwIO e

data NetworkTraceEvent = NetworkTraceEvent
  { nteRunId :: Int,
    nteSender :: String,
    nteReceiver :: String,
    nteRecvLogicalTime :: Int,
    nteSentLogicalTime :: Int
  }

instance FromRow NetworkTraceEvent where
  fromRow = NetworkTraceEvent <$> field <*> field <*> field <*> field <*> field

sqliteLoad :: TestId -> IO [Trace]
sqliteLoad testId = do
  path <- getDbPath
  conn <- open path
  r <-
    queryNamed
      conn
      "SELECT run_id,sender,receiver,recv_logical_time,sent_logical_time FROM network_trace \
      \ WHERE test_id = :testId \
      \ AND kind <> 'timer' \
      \ AND NOT dropped \
      \ AND NOT (sender   LIKE 'client:%') \
      \ AND NOT (receiver LIKE 'client:%') \
      \ ORDER BY run_id ASC"
      [":testId" := testId] ::
      IO [NetworkTraceEvent]
  return (historyToTrace (groupBy (\e1 e2 -> nteRunId e1 == nteRunId e2) r))
  where
    historyToTrace :: [[NetworkTraceEvent]] -> [Trace]
    historyToTrace = map (map go)
      where
        go :: NetworkTraceEvent -> Event
        go (NetworkTraceEvent _runId sender receiver recvAt sentAt) =
          Event sender (toEnum sentAt) receiver (toEnum recvAt)

-- TODO(stevan): What exactly do we need to store? Previous faults are no longer
-- interesting.
sqliteStore :: LdfiEvent -> IO ()
sqliteStore = undefined

sqliteStorage :: Storage IO
sqliteStorage =
  Storage
    { load = sqliteLoad,
      store = sqliteStore
    }
