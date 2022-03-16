module Dumblog.ZeroCopy.Main where

import Control.Concurrent (MVar)
import Control.Concurrent.Async (link, withAsync)
import Journal (allocateJournal, defaultOptions, startJournal)
import Journal.Internal.Logger (nullLogger)
import Journal.Types
       (Subscriber(Sub2), oLogger, oMaxSubscriber, oTermBufferLength)

import Dumblog.ZeroCopy.HttpServer
import Dumblog.ZeroCopy.State
import Dumblog.ZeroCopy.Worker

------------------------------------------------------------------------

zeroCopyDumblog :: Int -> Int -> FilePath -> Maybe (MVar ()) -> IO ()
zeroCopyDumblog capacity port fp mReady = do
  let opts = defaultOptions
               { oTermBufferLength = capacity
               , oLogger = nullLogger
               -- NOTE: A second subscriber whose bytes consumed is never
               -- incremented is used to avoid cleaning up the journal,
               -- otherwise we can have reads that point to a cleaned up part of
               -- the journal. A better longer term solution would be to have
               -- the compaction phase of the journal store the data somewhere
               -- more long term and update the in-memory location to point to
               -- that instead and allow the journal to be cleaned up.
               , oMaxSubscriber = Sub2
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
