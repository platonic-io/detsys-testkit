module Dumblog.ZeroCopy.Main where

import Journal.Types (Journal)
import Journal (defaultOptions, allocateJournal, startJournal)

import Dumblog.ZeroCopy.HttpServer

------------------------------------------------------------------------

zeroCopyDumblog :: Journal -> Int -> IO ()
zeroCopyDumblog jour port = httpServer jour port

main :: IO ()
main = do
  let fp   = "/tmp/dumblog-zero-copy.journal"
      opts = defaultOptions
  allocateJournal fp opts
  jour <- startJournal fp opts
  zeroCopyDumblog jour 8054
