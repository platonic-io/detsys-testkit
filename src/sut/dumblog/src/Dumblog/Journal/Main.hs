{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
module Dumblog.Journal.Main where

import Control.Concurrent.Async (withAsync, link)
import Control.Concurrent.MVar (MVar)
import qualified Data.Aeson as Aeson
import Data.Vector (Vector)
import qualified Data.Vector as Vector
import Debugger.State (InstanceStateRepr(..), DebEvent(..))
import Journal (defaultOptions, allocateJournal, startJournal)
import Journal.Types (Journal, Options, Subscriber(..), oLogger, oMaxSubscriber, readBytesConsumed, writeBytesConsumed, jMetadata)
import qualified Journal.MP as Journal
import Journal.Internal.Logger as Logger
import qualified Journal.Internal.Metrics as Metrics
import Options.Generic

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

-- TODO: we should just reads until we hit `before`
fetchJournal :: Maybe Snapshot -> FilePath -> Options -> IO Journal
fetchJournal mSnapshot fpj opts = do
  allocateJournal fpj opts
  journal <- startJournal fpj opts
  case mSnapshot of
    Nothing -> pure ()
    Just snap -> do
      let bytes = Snapshot.ssBytesInJournal snap
      before <- readBytesConsumed (jMetadata journal) Sub1
      putStrLn $ "[journal] Found Snapshot! starting from bytes: "  <> show bytes <> " to: " <> show before
      writeBytesConsumed (jMetadata journal) Sub1 bytes
  pure journal

-- this can be pure when `runCommand` gets pure
replay :: [Command] -> InMemoryDumblog -> IO InMemoryDumblog
replay [] s = do
  putStrLn "[REPLAY] finished!"
  pure s
replay (cmd:cmds) s = do
  putStrLn $ "[REPLAY] running: " <> show cmd
  (s', _) <- runCommand s cmd
  replay cmds s'

type DebugFile = Vector InstanceStateRepr

-- TODO: merge with `replay`
replayDebug :: [Command] -> InMemoryDumblog -> IO DebugFile
replayDebug = go 0 mempty
  where
    go _logTime dfile [] _s = do
      putStrLn "[REPLAY-DEBUG] finished!"
      pure dfile
    go logTime dfile (cmd:cmds) s = do
      putStrLn $ "[REPLAY-DEBUG] running: " <> show cmd
      (s', _) <- runCommand s cmd
      let
        ce = DebEvent
          { from = "client"
          , to = "dumblog"
          , event = "event?"
          , receivedLogical = logTime
          , message = "message?"
          }
        is = InstanceStateRepr
             { state = "diff"
             , currentEvent = ce
             , logs = []
             , sent = []
             }
      go (succ logTime) (Vector.snoc dfile is) cmds s'

collectAll :: Journal -> IO [Command]
collectAll jour = do
  putStrLn "[collect] Checking journal for old-entries"
  val <- Journal.readJournal jour Sub1
  case val of
    Nothing -> do
      putStrLn "[collect] No more entries"
      pure []
    Just entry -> do
      putStrLn "[collect] Found an entry"
      let Envelope _key cmd = decode entry
      cmds <- collectAll jour
      pure $ cmd : cmds

startingState :: Maybe Snapshot -> InMemoryDumblog
startingState Nothing = initState
startingState (Just snap) = Snapshot.ssState snap

data DumblogConfig
  = Run
  | DebugFile
    { output :: FilePath <?> "Where to output the debug file"
    }
  deriving (Generic, Show)

instance ParseRecord DumblogConfig

{-
Unclear how to:
* How to archive the journal
-}
journalDumblog :: DumblogConfig -> Int -> Int -> Maybe (MVar ()) -> IO ()
journalDumblog cfg _capacity port mReady = do
  let fpj = "/tmp/dumblog.journal"
      fpm = "/tmp/dumblog.metrics"
      fps = "/tmp/dumblog.snapshot"
      opts = defaultOptions { oLogger = Logger.nullLogger
                            , oMaxSubscriber = Sub2
                            }
      untilSnapshot = 1000
  case cfg of
    Run -> do
      mSnapshot <- Snapshot.readFile fps
      journal <- fetchJournal mSnapshot fpj opts
      metrics <- Metrics.newMetrics dumblogSchema fpm
      blocker <- emptyBlocker 0 -- it is okay to start over
      cmds <- collectAll journal
      workerState <- replay cmds (startingState mSnapshot)
      let
        events = length cmds
        feInfo = FrontEndInfo blocker
        wInfo = WorkerInfo blocker fps events untilSnapshot
      withAsync (worker journal metrics wInfo workerState) $ \a -> do
        link a
        runFrontEnd port journal feInfo mReady
    DebugFile fp -> do
      mSnapshot <- Snapshot.readFile fps
      journal <- fetchJournal mSnapshot fpj opts
      cmds <- collectAll journal
      debugFileContents <- replayDebug cmds (startingState mSnapshot)
      Aeson.encodeFile (unHelpful fp) debugFileContents
      putStrLn "Generated Debug-file"
