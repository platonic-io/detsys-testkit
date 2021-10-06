{-# LANGUAGE ScopedTypeVariables #-}

module Scheduler.Main where

import Control.Concurrent.Async
import System.FilePath ((</>))
import System.IO.Error (catchIOError, isDoesNotExistError)
import System.Environment (getEnv)
import Control.Exception (throwIO)

import StuntDouble
import Scheduler
import Scheduler.State
import Scheduler.Executor

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

main :: String -> IO ()
main version = do
  let executorPort = 3001
      executorRef = RemoteRef ("http://localhost:" ++ show executorPort ++ "/api/v1/event") 0
      schedulerPort = 3005
  fp <- getDbPath
  el <- makeEventLoop realTime (makeSeed 0) HttpSync (AdminNamedPipe "/tmp/")
          executorCodec (RealDisk fp) (EventLoopName "scheduler")
  now <- getCurrentTime realTime
  lref <- spawn el (fakeScheduler executorRef) (initState now (makeSeed 0))
  withHttpFrontend el lref schedulerPort $ \pid -> do
    putStrLn ("Scheduler (version " ++ version ++ ") is listening on port: " ++ show schedulerPort)
    waitForEventLoopQuit el
    cancel pid
