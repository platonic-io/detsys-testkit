module Dumblog.Common.Constants where

import Journal (defaultOptions)
import Journal.Internal.Logger as Logger
import Journal.Types (Options, Subscriber(..), oLogger, oMaxSubscriber, oTermBufferLength)

dUMBLOG_PORT :: Int
dUMBLOG_PORT = 8054

dumblogOptions :: Options
dumblogOptions = defaultOptions
  { oLogger = Logger.nullLogger
  , oMaxSubscriber = Sub2
  , oTermBufferLength = 512 * 1024 * 1024
  }

dumblogJournalPath :: Int -> FilePath
dumblogJournalPath portUsed = "/tmp/dumblog-" ++ show portUsed ++ ".journal"

dumblogSnapshotPath :: Int -> FilePath
dumblogSnapshotPath portUsed = "/tmp/dumblog-" ++ show portUsed ++ ".snapshot"
