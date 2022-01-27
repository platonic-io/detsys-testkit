{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (async)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSChar8
import qualified Data.ByteString.Lazy as LBS
import Data.Time (getCurrentTime, diffUTCTime)

import Network.HTTP.Types.Status (status200)
import qualified Network.Wai as Wai
import Network.Wai.Handler.Warp

import Journal (Journal)
import qualified Journal
import Journal.Internal.Metrics (MetricsSchema, Metrics) -- should maybe be moved to separate package
import qualified Journal.Internal.Metrics as Metrics

data DumblogCounters
  = CurrentNumberTransactions
  | NumberOfWrites
  | NumberOfReads
  | ErrorsEncountered
  deriving (Eq, Show, Enum, Bounded)

data DumblogHistograms
  = ResponseTime
  deriving (Eq, Show, Enum, Bounded)

type DumblogMetrics = Metrics DumblogCounters DumblogHistograms

dumblogSchema :: MetricsSchema DumblogCounters DumblogHistograms
dumblogSchema = Metrics.MetricsSchema 1

httpFrontend :: Journal -> Wai.Application
httpFrontend journal req respond = do
  body <- Wai.strictRequestBody req
  -- we need to be able to know what command this is so that we can wait for the appropriate response
  Journal.appendBS journal (LBS.toStrict body)
  putStrLn $ "Added to journal: " <> show body
  Journal.dumpJournal journal
  respond $ Wai.responseLBS status200 [] "Added to journal"

data Command
  = Write ByteString
  | Read Int

parseCommand :: ByteString -> IO (Maybe Command)
parseCommand _ = pure Nothing

-- This is the main state of Dumblog, which is the result of applying all commands in the log
data InMemoryDumblog = InMemoryDumblog
  { theLog :: [ByteString] -- not very memory efficient, but not the point
  }

initState = InMemoryDumblog []

type Response = LBS.ByteString

runCommand :: InMemoryDumblog -> Command -> IO (InMemoryDumblog, Response)
runCommand s _ = pure (s, error "Not implemented")

-- Currently always uses `ResponseTime`
timeIt :: DumblogMetrics -> IO a -> IO a
timeIt metrics action = do
  startTime <- getCurrentTime
  result <- action
  endTime <- getCurrentTime
  -- dunno what timescale we are measuring
  Metrics.measure metrics ResponseTime (realToFrac . (*1000) $ diffUTCTime endTime startTime)
  return result


worker :: Journal -> DumblogMetrics -> InMemoryDumblog -> IO ()
worker journal metrics = go
  where
    go s = do
      { val <- Journal.readJournal journal
      ; s' <- case val of
        { Nothing -> return s
        ; Just entry -> timeIt metrics $ do
          mcmd <- parseCommand entry
          case mcmd of
            Nothing -> do
              Metrics.incrCounter metrics ErrorsEncountered 1
              return s
            Just cmd -> do
              (s', r) <- runCommand s cmd
              -- we should return r somehow?
              return s'
        }
      ; threadDelay 10
      ; go s'
      }

{-
Unclear how to:
* How to respond to synchronous client calls
* How to archive the journal
* How to read from journal in a blocking way?
  - The journal should be the thing that decides order
  - But we seem to only have one reader, should that be the worker or some archiver?
* How to properly use the files?
  - If the files don't exists we should create them
  - If they exists we should not allocate (unless explicitly asked to)
* How do snapshots work?
-}
main :: IO ()
main = do

  let fpj = "/tmp/dumblog.journal"
      fpm = "/tmp/dumblog.metrics"
      opts = Journal.defaultOptions
  Journal.allocateJournal fpj opts -- should we really allocate?
  journal <- Journal.startJournal fpj opts
  metrics <- Metrics.newMetrics dumblogSchema fpm
  let state = initState -- shis should be from snapshot/or replay journal
  async $ worker journal metrics state
  run 8053 (httpFrontend journal)
