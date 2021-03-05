{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Ldfi.Storage where

import Control.Exception
import Data.Aeson (decode)
import qualified Data.Binary.Builder as BB
import Data.List (groupBy, intercalate)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TextE
import Database.SQLite.Simple
import qualified Ldfi.Marshal.Faults as MF
import Ldfi.Traces
import System.Environment
import System.FilePath
import System.IO.Error

------------------------------------------------------------------------

type TestId = Int

type RunId = Int

data Fault = Crash Node Time | Omission Edge Time
  deriving (Eq, Ord, Read, Show)

type Failures = [Fault]

data LdfiEvent = LdfiEvent
  { leTestId :: TestId,
    leRunIds :: [RunId],
    leFaults :: [String], -- XXX: Fault?
    leVersion :: String,
    leStatistics :: String
  }

data TestInformation = TestInformation
  { tiTestId :: TestId,
    tiFailedRuns :: [RunId]
  }

data Storage m = Storage
  { load :: TestInformation -> m [Trace],
    loadFailures :: TestInformation -> m [Failures],
    store :: LdfiEvent -> m ()
  }

mockStorage :: Monad m => [Trace] -> Storage m
mockStorage ts =
  Storage
    { load = const (return ts),
      loadFailures = const (return []),
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

sqliteShowSequence :: [Int] -> String
sqliteShowSequence xs = "(" ++ intercalate ", " (map show xs) ++ ")"

sqliteLoad :: TestInformation -> IO [Trace]
sqliteLoad testInformation = do
  path <- getDbPath
  conn <- open path
  let
    testId = tiTestId testInformation
    failedRuns = Set.fromList $ tiFailedRuns testInformation
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
  return (historyToTrace (groupBy (\e1 e2 -> nteRunId e1 == nteRunId e2) . filter (not . flip Set.member failedRuns . nteRunId) $ r))
  where
    historyToTrace :: [[NetworkTraceEvent]] -> [Trace]
    historyToTrace = map (map go)
      where
        go :: NetworkTraceEvent -> Event
        go (NetworkTraceEvent _runId sender receiver recvAt sentAt) =
          Event sender (toEnum sentAt) receiver (toEnum recvAt)

sqliteLoadFailure :: TestInformation -> IO [Failures]
sqliteLoadFailure testInformation = do
  path <- getDbPath
  conn <- open path
  let
    testId = tiTestId testInformation
    failedRuns = Set.fromList $ tiFailedRuns testInformation
  r <-
    queryNamed
      conn
      "SELECT run_id, faults FROM run_info WHERE test_id = :testId"
      [":testId" := testId] ::
      IO [(RunId, Text)]
  return . map (parse . snd) . filter (flip Set.member failedRuns . fst) $ r
  where
    parse :: Text -> Failures
    parse s = case decode (BB.toLazyByteString $ TextE.encodeUtf8Builder s) of
      Nothing -> error $ "Unable to parse faults: " ++ Text.unpack s
      Just x  -> map convert x
    convert :: MF.Fault -> Fault
    convert (MF.Omission f t a) = Omission (f,t) a
    convert (MF.Crash f a) = Crash f a

-- TODO(stevan): What exactly do we need to store? Previous faults are no longer
-- interesting.
sqliteStore :: LdfiEvent -> IO ()
sqliteStore = undefined

sqliteStorage :: Storage IO
sqliteStorage =
  Storage
    { load = sqliteLoad,
      loadFailures = sqliteLoadFailure,
      store = sqliteStore
    }
