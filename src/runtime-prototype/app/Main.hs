{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Control.Concurrent.Async (cancel)
import Control.Exception (throwIO)
import System.Environment (getEnv)
import System.FilePath ((</>))
import System.IO.Error (catchIOError, isDoesNotExistError)

import Scheduler
import StuntDouble

------------------------------------------------------------------------

getDbPath :: IO FilePath
getDbPath = do
  getEnv "DETSYS_DB"
    `catchIOError` \(e :: catchIOError) ->
      if isDoesNotExistError e
        then do
          home <- getEnv "HOME"
          return (home </> ".detsys.db")
        else throwIO e

main :: IO ()
main = do
  let executorPort = 3001
      executorRef = RemoteRef ("http://localhost:" ++ show executorPort ++ "/api/v1/event") 0
      schedulerPort = 3005
  fp <- getDbPath
  el <- makeEventLoop realTime (makeSeed 0) HttpSync (AdminNamedPipe "/tmp/")
          executorCodec (RealDisk fp) (EventLoopName "scheduler")
  now <- getCurrentTime realTime
  lref <- spawn el (fakeScheduler executorRef) (initState now (makeSeed 0))
  withHttpFrontend el lref schedulerPort $ \pid -> do
    putStrLn ("Scheduler is listening on port: " ++ show schedulerPort)
    waitForEventLoopQuit el
    cancel pid
