module Dumblog.ZeroCopy.Main where

import Control.Concurrent (MVar)
import Journal.Types (oTermBufferLength, oLogger)
import Journal.Internal.Logger (nullLogger)
import Journal (defaultOptions, allocateJournal, startJournal)

import Dumblog.ZeroCopy.HttpServer

------------------------------------------------------------------------

zeroCopyDumblog :: Int -> Int -> Maybe (MVar ()) -> IO ()
zeroCopyDumblog capacity port mReady = do
  let fp   = "/tmp/dumblog-zero-copy.journal"
      opts = defaultOptions { oTermBufferLength = capacity
                            , oLogger = nullLogger
                            }
  allocateJournal fp opts
  jour <- startJournal fp opts
  httpServer jour port mReady

main :: IO ()
main = zeroCopyDumblog (512 * 1024 * 1024) 8054 Nothing
