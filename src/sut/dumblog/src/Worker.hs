{-# LANGUAGE OverloadedStrings #-}
module Worker where

import Control.Concurrent (threadDelay)
import Control.Monad (unless)
import qualified Data.Binary as Binary
import Data.ByteString (ByteString, uncons)
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Char as Char
import Data.Time (getCurrentTime, diffUTCTime)

import Journal (Journal)
import qualified Journal
import qualified Journal.Internal.Metrics as Metrics
import qualified Journal.Types.AtomicCounter as AtomicCounter

import Blocker
import Codec
import Metrics
import qualified Snapshot
import StateMachine
import Types

data WorkerInfo = WorkerInfo
  { wiBlockers :: Blocker (Either Response Response)
  , wiSnapshotFile  :: FilePath
  , wiEvents        :: Int -- how many events since last snapshot
  , wiEventsInRound :: Int -- how many events in one snapshot
  }

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
worker journal metrics (WorkerInfo blocker snapshotFile eventCount untilSnapshot) = go eventCount
  where
    go ev s
      | ev >= untilSnapshot = do
          putStrLn $ "[worker] Performing Snapshot"
          bytes <- AtomicCounter.readCounter $ Journal.jBytesConsumed journal
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
              wakeUpFrontend blocker key $ Left "Couldn't parse request" -- should be better error message
-}
          (s', r) <- runCommand s cmd
          wakeUpFrontend blocker key (Right r)
          return (succ ev, s')
        }
      ; threadDelay 10
      ; go ev' s'
      }
