{-# LANGUAGE OverloadedStrings #-}
module Worker where

import Control.Concurrent (threadDelay)
import Control.Monad (unless)
import Data.ByteString (ByteString)
import Data.Time (getCurrentTime, diffUTCTime)

import Journal (Journal)
import qualified Journal
import qualified Journal.Internal.Metrics as Metrics

import Blocker
import Codec
import Metrics
import StateMachine
import Types

parseCommand :: ByteString -> IO (Maybe Command)
parseCommand bs = do
  pure Nothing

data WorkerInfo = WorkerInfo
  { wiBlockers :: Blocker (Either Response Response)
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
worker journal metrics (WorkerInfo blocker) = go
  where
    go s = do
      { val <- Journal.readJournal journal
      ; s' <- case val of
        { Nothing -> return s
        ; Just entry -> timeIt metrics $ do
          let Envelope key cmdString = decode entry
          mcmd <- parseCommand cmdString
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
