module Dumblog.ZeroCopy.Main where

import Control.Concurrent (MVar)
import Control.Concurrent.Async (link, withAsync)
import Journal (allocateJournal, defaultOptions, startJournal)
import Journal.Internal.Logger (nullLogger)
import Journal.Types (oLogger, oTermBufferLength)

import Dumblog.ZeroCopy.HttpServer
import Dumblog.ZeroCopy.State
import Dumblog.ZeroCopy.Worker

------------------------------------------------------------------------

zeroCopyDumblog :: Int -> Int -> FilePath -> Maybe (MVar ()) -> IO ()
zeroCopyDumblog capacity port fp mReady = do
  let opts = defaultOptions { oTermBufferLength = capacity
                            , oLogger = nullLogger
                            }
  allocateJournal fp opts
  jour <- startJournal fp opts
  state <- initState 40000 fp
  withAsync (worker jour state) $ \a -> do
    link a
    httpServer jour port mReady

main :: IO ()
main = do
  let fp = "/tmp/dumblog-zero-copy.journal"
  zeroCopyDumblog (512 * 1024 * 1024) 8054 fp Nothing
