{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

module Dumblog.Journal.Worker where

import Control.Concurrent (threadDelay)
import Control.Monad (unless)
import qualified Data.ByteString as BS
import Data.Int (Int64)
import qualified Journal.Internal.Metrics as Metrics
import qualified Journal.MP as Journal
import Journal.Types
       ( Journal
       , Subscriber(..)
       , jMetadata
       , readBytesConsumed
       , writeBytesConsumed
       )

import Dumblog.Common.Metrics
import Dumblog.Journal.Blocker
import Dumblog.Journal.Codec
import Dumblog.Journal.Logger
import qualified Dumblog.Journal.Snapshot as Snapshot
import Dumblog.Journal.StateMachine hiding (runCommand)
import Dumblog.Journal.Types
import Dumblog.Journal.Versions.Codec (runCommand)

------------------------------------------------------------------------

data WorkerInfo = WorkerInfo
  { wiBlockers :: Blocker Response
  , wiLogger :: Logger
  , wiSnapshotFile  :: FilePath
  , wiCurrentVersion :: Int64
  , wiEvents        :: Int -- how many events since last snapshot
  , wiEventsInRound :: Int -- how many events in one snapshot
  }

wakeUpFrontend :: Blocker response -> Int -> response -> IO ()
wakeUpFrontend blocker key resp = do
  b <- wakeUp blocker key resp
  unless b $
    error $ "Frontend never added MVar"

worker :: Journal -> DumblogMetrics -> WorkerInfo -> InMemoryDumblog -> IO ()
worker journal metrics (WorkerInfo blocker logger snapshotFile currentVersion eventCount untilSnapshot) =
  go eventCount
  where
    go :: Int -> InMemoryDumblog -> IO ()
    go ev s = do
      if ev >= untilSnapshot
      then do
        logger "[worker] Performing Snapshot"
        bytes <- readBytesConsumed (jMetadata journal) Sub1
        Snapshot.toFile (Snapshot.Snapshot bytes currentVersion s) snapshotFile
        writeBytesConsumed (jMetadata journal) Sub2 bytes
        go 0 s
      else do
        mEntry <- Journal.readJournal journal Sub1
        case mEntry of
          Nothing -> threadDelay 0 >> go ev s
          Just entry -> do
            Metrics.decrCounter_ metrics QueueDepth 1
            let Envelope key cmd version arrivalTime = decode entry
            -- XXX: In case of decode error:
            --  Metrics.incrCounter metrics ErrorsEncountered 1
            --  wakeUpFrontend blocker key $ Left "Couldn't parse request"
            --  -- ^ should be better error message
            --
            !startTime <- getCurrentNanosSinceEpoch
            (s', r) <- runCommand version logger s cmd
            wakeUpFrontend blocker key r
            !endTime <- getCurrentNanosSinceEpoch
            -- Convert from nano s to µs with `* 10^-3`.
            let latency     = realToFrac ((startTime - arrivalTime)) * 0.001 -- µs.
                serviceTime = realToFrac ((endTime   - startTime))   * 0.001
            Metrics.measure metrics Latency latency
            Metrics.measure metrics (case cmd of
                                       Write {} -> ServiceTimeWrites
                                       Read {}  -> ServiceTimeReads) serviceTime
            Metrics.measure metrics ResponseTime (latency + serviceTime)
            case cmd of
              Write bs   -> Metrics.measure metrics WriteSize (realToFrac (BS.length bs))
              _otherwise -> return ()
            go (ev + 1) s'
