{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

module Dumblog.Journal.Worker where

import Control.Concurrent (threadDelay)
import Control.Monad (unless)
import Data.Time (diffUTCTime, getCurrentTime)

import qualified Journal.Internal.Metrics as Metrics
import qualified Journal.MP as Journal
import Journal.Types (Journal, jMetadata, readBytesConsumed)

import Dumblog.Journal.Blocker
import Dumblog.Journal.Codec
import Dumblog.Journal.Metrics
import qualified Dumblog.Journal.Snapshot as Snapshot
import Dumblog.Journal.StateMachine
import Dumblog.Journal.Types

------------------------------------------------------------------------

data WorkerInfo = WorkerInfo
  { wiBlockers :: Blocker (Either Response Response)
  , wiSnapshotFile  :: FilePath
  , wiEvents        :: Int -- how many events since last snapshot
  , wiEventsInRound :: Int -- how many events in one snapshot
  }

-- Currently always uses `ResponseTime`
timeIt :: DumblogMetrics -> IO a -> IO a
timeIt metrics action = do
  !startTime <- getCurrentTime
  result <- action
  !endTime <- getCurrentTime
  -- dunno what timescale we are measuring
  Metrics.measure metrics ServiceTime (realToFrac . (*1000) $ diffUTCTime endTime startTime)
  return result

wakeUpFrontend :: Blocker (Either Response Response) -> Int -> Either Response Response
               -> IO ()
wakeUpFrontend blocker key resp = do
  b <- wakeUp blocker key resp
  unless b $
    error $ "Frontend never added MVar"

worker :: Journal -> DumblogMetrics -> WorkerInfo -> InMemoryDumblog -> IO ()
worker journal metrics (WorkerInfo blocker snapshotFile eventCount untilSnapshot) =
  go eventCount
  where
    go ev s
      | ev >= untilSnapshot = do
          putStrLn $ "[worker] Performing Snapshot"
          bytes <- readBytesConsumed (jMetadata journal)
          Snapshot.toFile (Snapshot.Snapshot bytes s) snapshotFile
          go 0 s
    go ev s = do
      { val <- Journal.readJournal journal
      ; (ev', s') <- case val of
        { Nothing -> return (ev, s)
        ; Just entry -> timeIt metrics $ do
          let Envelope key cmd = decode entry
          {- // in case of decode error
              Metrics.incrCounter metrics ErrorsEncountered 1
              wakeUpFrontend blocker key $ Left "Couldn't parse request"
              -- ^ should be better error message
          -}
          -- putStrLn ("worker: key: " ++ show key ++ ", cmd: " ++ show cmd)
          (s', r) <- runCommand s cmd
          -- putStrLn ("worker: key: " ++ show key ++ ", response: " ++ show r)
          wakeUpFrontend blocker key (Right r)
          return (succ ev, s')
        }
      ; threadDelay 10
      ; go ev' s'
      }
