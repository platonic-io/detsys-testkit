module Dumblog.Journal.Main where

import Control.Concurrent.Async (withAsync, link)
import Control.Concurrent.MVar (MVar)
import Journal (defaultOptions, allocateJournal, startJournal)
import Journal.Types (Journal, Options, oLogger, writeBytesConsumed, jMetadata)
import qualified Journal.MP as Journal
import Journal.Internal.Logger as Logger
import qualified Journal.Internal.Metrics as Metrics
import qualified Journal.Types.AtomicCounter as AtomicCounter

import Dumblog.Journal.Blocker (emptyBlocker)
import Dumblog.Journal.Codec (Envelope(..), decode)
import Dumblog.Journal.FrontEnd (FrontEndInfo(..), runFrontEnd)
import Dumblog.Journal.Metrics (dumblogSchema)
import Dumblog.Journal.Snapshot (Snapshot)
import qualified Dumblog.Journal.Snapshot as Snapshot
import Dumblog.Journal.StateMachine
       (InMemoryDumblog, initState, runCommand)
import Dumblog.Journal.Types (Command)
import Dumblog.Journal.Worker (WorkerInfo(..), worker)

------------------------------------------------------------------------

fetchJournal :: Maybe Snapshot -> FilePath -> Options -> IO Journal
fetchJournal mSnapshot fpj opts = do
  allocateJournal fpj opts
  journal <- startJournal fpj opts
  case mSnapshot of
    Nothing -> pure ()
    Just snap -> do
      let bytes = Snapshot.ssBytesInJournal snap
      putStrLn $ "[journal] Found Snapshot! starting from bytes: "  <> show bytes
      writeBytesConsumed (jMetadata journal) bytes
  pure journal

-- this should be from snapshot/or replay journal
fetchState :: Maybe Snapshot -> Journal -> IO (InMemoryDumblog, Int)
fetchState mSnapshot jour0 = do
  cmds <- collectAll jour0 -- the journal has been set to be either 0, or from
                           -- the last snapshot
  s <- replay cmds startingState
  pure (s, length cmds)
  where
    startingState = case mSnapshot of
      Nothing -> initState
      Just snap -> Snapshot.ssState snap
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
-}
journalDumblog :: Int -> Int -> Maybe (MVar ()) -> IO ()
journalDumblog _capacity port mReady = do
  let fpj = "/tmp/dumblog.journal"
      fpm = "/tmp/dumblog.metrics"
      fps = "/tmp/dumblog.snapshot"
      opts = defaultOptions { oLogger = Logger.nullLogger }
      untilSnapshot = 1000
  mSnapshot <- Snapshot.readFile fps
  journal <- fetchJournal mSnapshot fpj opts
  metrics <- Metrics.newMetrics dumblogSchema fpm
  blocker <- emptyBlocker
  counter <- AtomicCounter.newCounter 0 -- it is okay to start over
  (state, events) <- fetchState mSnapshot journal
  let feInfo = FrontEndInfo counter blocker
      wInfo = WorkerInfo blocker fps events untilSnapshot
  withAsync (worker journal metrics wInfo state) $ \a -> do
    link a
    runFrontEnd port journal feInfo mReady
