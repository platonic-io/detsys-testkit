module Main where

import Control.Concurrent.Async (async)
import Control.Monad (when)
import Journal (Journal)
import qualified Journal
import qualified Journal.Types.AtomicCounter as AtomicCounter
import qualified Journal.Internal.Metrics as Metrics
import Journal.Internal.Logger as Logger

import Blocker (emptyBlocker)
import Codec (Envelope(..), decode)
import FrontEnd (runFrontEnd, FrontEndInfo(..))
import Metrics (dumblogSchema)
import StateMachine(InMemoryDumblog, initState, runCommand)
import Types (Command)
import Worker (worker, WorkerInfo(..))

fetchJournal :: FilePath -> Journal.Options -> IO Journal
fetchJournal fpj opts = do
  Journal.allocateJournal fpj opts
  Journal.startJournal fpj opts

-- this should be from snapshot/or replay journal
fetchState :: Journal -> IO InMemoryDumblog
fetchState jour = do
  cmds <- collectAll jour -- the journal start from 0 -- we don't know where we were
  replay cmds initState
  where
    -- maybe better stream type than []?
    -- this is not the best performance, but it is only in startup and should be replaced
    -- later anyway..
    collectAll :: Journal -> IO [Command]
    collectAll jour = do
      putStrLn "[collect] Checking journal for old-entries"
      val <- Journal.readJournal jour
      case val of
        Nothing -> do
          putStrLn "[collect] No more entries"
          pure []
        Just entry -> do
          putStrLn "[collect] Found an entry"
          let Envelope _key cmd = decode entry
          cmds <- collectAll jour
          pure $ cmd : cmds

    -- this can be pure when `runCommand` gets pure
    replay :: [Command] -> InMemoryDumblog -> IO InMemoryDumblog
    replay [] s = do
      putStrLn "[REPLAY] finished!"
      pure s
    replay (cmd:cmds) s = do
      putStrLn $ "[REPLAY] running: " <> show cmd
      (s', _) <- runCommand s cmd
      replay cmds s'

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
      opts = Journal.defaultOptions { Journal.oLogger = Logger.nullLogger }

  journal <- fetchJournal fpj opts
  metrics <- Metrics.newMetrics dumblogSchema fpm
  blocker <- emptyBlocker
  counter <- AtomicCounter.newCounter 0 -- it is okay to start over
  state <- fetchState journal
  let feInfo = FrontEndInfo counter blocker
      wInfo = WorkerInfo blocker
  async $ worker journal metrics wInfo state
  runFrontEnd 8053 journal feInfo
