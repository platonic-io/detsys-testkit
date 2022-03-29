{-# LANGUAGE OverloadedStrings #-}

module Debugger.Main where

import Brick.BChan
import Control.Concurrent
import Control.Exception (finally)
import Data.Aeson (decodeFileStrict)
import Data.Time.LocalTime (getCurrentTimeZone)
import System.Environment (getArgs)
import System.Exit (die)

import Debugger.State
import Debugger.UI (AppEvent(UpdateState), mkAppState, runApp)

------------------------------------------------------------------------

debugMain :: IO ()
debugMain = do
  as <- getArgs
  case as of
    ["watch", fp] -> do
      content <- decodeFileStrict fp
      case content of
        Nothing -> die "Can't parse debug-file"
        Just values -> do
          bchan <- newBChan 1024
          tid <- forkIO (watch fp bchan)
          tz <- getCurrentTimeZone
          runApp (mkAppState tz (fromRepr values)) (Just bchan)
            `finally` killThread tid
    [f] -> do
      content <- decodeFileStrict f
      case content of
        Nothing -> do die "Can't parse debug-file"
        Just values -> do
          tz <- getCurrentTimeZone
          runApp (mkAppState tz (fromRepr values)) Nothing
    _ -> die "Debugger wants exactly one input, which is the debug-file"

watch :: FilePath -> BChan AppEvent -> IO ()
watch fp bchan = go
  where
    go :: IO ()
    go = do
      threadDelay 500000
      content <- decodeFileStrict fp
      case content of
        Nothing -> die "Can't parse debug-file"
        Just values -> do
          writeBChan bchan (UpdateState (fromRepr values))
          go
