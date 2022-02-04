{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (async)
import Control.Monad (unless)
import qualified Data.Binary as Binary
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSChar8
import qualified Data.ByteString.Lazy as LBS
import Data.Time (getCurrentTime, diffUTCTime)

import Network.HTTP.Types.Status (status200, status400)
import qualified Network.Wai as Wai
import Network.Wai.Handler.Warp

import Journal (Journal)
import qualified Journal
import Journal.Types.AtomicCounter (AtomicCounter)
import qualified Journal.Types.AtomicCounter as AtomicCounter
import Journal.Internal.Metrics (MetricsSchema, Metrics) -- should maybe be moved to separate package
import qualified Journal.Internal.Metrics as Metrics

import Blocker
import Metrics

data FrontEndInfo = FrontEndInfo
  { sequenceNumber :: AtomicCounter
  , blockers :: Blocker (Either Response Response)
  }

httpFrontend :: Journal -> FrontEndInfo -> Wai.Application
httpFrontend journal (FrontEndInfo c blocker) req respond = do
  body <- Wai.strictRequestBody req
  key <- AtomicCounter.incrCounter 1 c
  Journal.appendBS journal (LBS.toStrict $ Binary.encode (key, body))
  resp <- blockUntil blocker key
  Journal.dumpJournal journal
  case resp of
    Left errMsg -> respond $ Wai.responseLBS status400 [] errMsg
    Right msg -> respond $ Wai.responseLBS status200 [] msg

data Command
  = Write ByteString
  | Read Int

parseCommand :: ByteString -> IO (Int, Maybe Command)
parseCommand bs = do
  let
    cmd :: LBS.ByteString
    (key, cmd) = Binary.decode $ LBS.fromStrict bs
  pure (key, Nothing)

-- This is the main state of Dumblog, which is the result of applying all commands in the log
data InMemoryDumblog = InMemoryDumblog
  { theLog :: [ByteString] -- not very memory efficient, but not the point
  }

initState = InMemoryDumblog []

data WorkerInfo = WorkerInfo
  { wiBlockers :: Blocker (Either Response Response)
  }

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

wakeUpFrontend :: Blocker (Either Response Response) -> Int -> Either Response Response -> IO ()
wakeUpFrontend blocker key resp = do
  b <- wakeUp blocker key resp
  unless b $
    error $ "Frontend never added MVar"

worker :: Journal -> DumblogMetrics -> WorkerInfo -> InMemoryDumblog -> IO ()
worker journal metrics (WorkerInfo blocker) = go
  where
    go s = do
      { val <- Journal.readJournal journal
      ; s' <- case val of
        { Nothing -> return s
        ; Just entry -> timeIt metrics $ do
          (key, mcmd) <- parseCommand entry
          case mcmd of
            Nothing -> do
              Metrics.incrCounter metrics ErrorsEncountered 1
              wakeUpFrontend blocker key $ Left "Couldn't parse request" -- should be better error message
              return s
            Just cmd -> do
              (s', r) <- runCommand s cmd
              wakeUpFrontend blocker key (Right r)
              return s'
        }
      ; threadDelay 10
      ; go s'
      }

{-
Unclear how to:
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
  blocker <- emptyBlocker
  counter <- AtomicCounter.newCounter 0
  let state = initState -- shis should be from snapshot/or replay journal
      feInfo = FrontEndInfo counter blocker
      wInfo = WorkerInfo blocker
  async $ worker journal metrics wInfo state
  run 8053 (httpFrontend journal feInfo)
