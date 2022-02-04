module Main where

import Control.Concurrent.Async (async)
import qualified Data.Binary as Binary
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSChar8
import qualified Data.ByteString.Lazy as LBS

import Journal (Journal)
import qualified Journal
import Journal.Types.AtomicCounter (AtomicCounter)
import qualified Journal.Types.AtomicCounter as AtomicCounter
import Journal.Internal.Metrics (MetricsSchema, Metrics) -- should maybe be moved to separate package
import qualified Journal.Internal.Metrics as Metrics

import Blocker
import FrontEnd
import Metrics
import StateMachine
import Types
import Worker


{-
Unclear how to:
* How to archive the journal
* How to read from journal in a blocking way?
  - The journal should be the thing that decides order
  - But we seem to only have one reader, should that be the worker or some archiver?
* How to properly use the files?
  - If the files don't exists we should create them
  - If they exists we should not allocate (unless explicitly asked to)
* How do snapshots work?
-}
main :: IO ()
main = do

  let fpj = "/tmp/dumblog.journal"
      fpm = "/tmp/dumblog.metrics"
      opts = Journal.defaultOptions
  Journal.allocateJournal fpj opts -- should we really allocate?
  journal <- Journal.startJournal fpj opts
  metrics <- Metrics.newMetrics dumblogSchema fpm
  blocker <- emptyBlocker
  counter <- AtomicCounter.newCounter 0
  let state = initState -- shis should be from snapshot/or replay journal
      feInfo = FrontEndInfo counter blocker
      wInfo = WorkerInfo blocker
  async $ worker journal metrics wInfo state
  runFrontEnd 8053 journal feInfo
