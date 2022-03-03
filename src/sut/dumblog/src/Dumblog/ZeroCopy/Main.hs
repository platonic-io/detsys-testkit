module Dumblog.ZeroCopy.Main where

import Control.Concurrent (MVar)
import Journal.Types (oTermBufferLength, oLogger)
import Journal.Internal.Logger (nullLogger)
import Journal (defaultOptions, allocateJournal, startJournal)

import Dumblog.ZeroCopy.HttpServer

------------------------------------------------------------------------

zeroCopyDumblog :: Int -> Int -> FilePath -> Maybe (MVar ()) -> IO ()
zeroCopyDumblog capacity port fp mReady = do
  let opts = defaultOptions { oTermBufferLength = capacity
                            , oLogger = nullLogger
                            }
  allocateJournal fp opts
  jour <- startJournal fp opts
  httpServer jour port mReady

main :: IO ()
main = do
  let fp = "/tmp/dumblog-zero-copy.journal"
  zeroCopyDumblog (512 * 1024 * 1024) 8054 fp Nothing
