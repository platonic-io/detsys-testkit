module Main where

import StuntDouble
import Scheduler

------------------------------------------------------------------------

main :: IO ()
main = do
  let executorPort = 3004
      executorRef = RemoteRef ("http://localhost:" ++ show executorPort) 0
  el <- makeEventLoop realTime (makeSeed 0) HttpSync (EventLoopName "scheduler")
  lref <- spawn el (fakeScheduler executorRef) initState
  putStrLn "Scheduler is running..."
  waitForEventLoopQuit el
