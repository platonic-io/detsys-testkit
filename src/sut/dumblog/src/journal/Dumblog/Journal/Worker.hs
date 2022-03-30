{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

module Dumblog.Journal.Worker where

import Control.Monad (unless)
import Data.Binary (decode)
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as LBS
import Data.Int (Int64)
import Data.Maybe (fromMaybe)
import qualified Journal.Internal.Metrics as Metrics
import qualified Journal.MP as Journal
import Journal.Types
       ( Journal
       , Subscriber(..)
       , jMetadata
       , readBytesConsumed
       , writeBytesConsumed
       )

import Dumblog.Common.HttpClient (ackHttp, backupHttp, newHttpClient)
import Dumblog.Common.Metrics
import Dumblog.Common.Types (SeqNum(..))
import Dumblog.Journal.Blocker
import Dumblog.Journal.Codec
import Dumblog.Journal.Logger
import qualified Dumblog.Journal.Snapshot as Snapshot
import Dumblog.Journal.StateMachine hiding (runCommand)
import Dumblog.Journal.Types
import Dumblog.Journal.Versions (runCommand)

------------------------------------------------------------------------

data WorkerInfo = WorkerInfo
  { wiBlockers :: Blocker ClientResponse
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
worker journal metrics wi = go (wiEvents wi)
  where
    go :: Int -> InMemoryDumblog -> IO ()
    go ev s = do
      if ev >= wiEventsInRound wi
      then do
        wiLogger wi "[worker] Performing Snapshot"
        bytes <- readBytesConsumed (jMetadata journal) Sub1
        Snapshot.toFile (Snapshot.Snapshot bytes (wiCurrentVersion wi) s) (wiSnapshotFile wi)
        writeBytesConsumed (jMetadata journal) Sub2 bytes
        go 0 s
      else do
        (n, s') <- Journal.readManyLazyJournalSC journal Sub1 s go'
        go (ev + n) s'

    go' :: InMemoryDumblog -> ByteString -> IO InMemoryDumblog
    go' s entry = do
      Metrics.decrCounter_ metrics QueueDepth 1
      let Envelope input version arrivalTime = decode entry
      -- XXX: In case of decode error:
      --  Metrics.incrCounter metrics ErrorsEncountered 1
      --  wakeUpFrontend blocker key $ Left "Couldn't parse request"
      --  -- ^ should be better error message
      --
      !startTime <- getCurrentNanosSinceEpoch
      (s', output) <- runCommand version (wiLogger wi) s input
      case output of
        ClientResponse resp (SeqNum key) -> do
          wakeUpFrontend (wiBlockers wi) key resp
          !endTime <- getCurrentNanosSinceEpoch
          -- Convert from nano s to µs with `* 10^-3`.
          let latency     = realToFrac ((startTime - arrivalTime)) * 0.001 -- µs.
              serviceTime = realToFrac ((endTime   - startTime))   * 0.001
          Metrics.measure metrics Latency latency
          case input of
            ClientRequest (Write {}) _ -> Metrics.measure metrics ServiceTimeWrites serviceTime
            ClientRequest (Read {})  _ -> Metrics.measure metrics ServiceTimeReads  serviceTime
            _otherwise -> return ()
          Metrics.measure metrics ResponseTime (latency + serviceTime)
          case input of
            ClientRequest (Write bs) _ -> Metrics.measure metrics WriteSize (realToFrac (LBS.length bs))
            _otherwise -> return ()

          return s'
        InternalMessageOut msg ->
          case msg of
            Ack ix sn -> do
              let port = fromMaybe (error "No peer port") (peerPort s')
              -- XXX: avoid creating a new http client every time...
              hc <- newHttpClient "localhost" port
              ackHttp hc ix sn
              return s'
            Backup ix bs sn -> do
              let port = fromMaybe (error "No peer port") (peerPort s')
              hc <- newHttpClient "localhost" port
              backupHttp hc ix bs sn
              return s'

        AdminResponse -> return s'
