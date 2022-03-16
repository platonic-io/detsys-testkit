{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}

module Dumblog.Journal.Main where

import Control.Concurrent.Async (link, withAsync)
import Control.Concurrent.MVar (MVar)
import qualified Data.Aeson as Aeson
import qualified Data.Text as Text
import Data.Text.Encoding (decodeUtf8)
import qualified Data.Text.Lazy as LText
import qualified Data.Text.Lazy.Encoding as LEncoding
import Data.Vector (Vector)
import qualified Data.Vector as Vector
import Debugger.State (DebEvent(..), InstanceStateRepr(..))
import Journal (allocateJournal, defaultOptions, startJournal)
import Journal.Internal.Logger as Logger
import qualified Journal.Internal.Metrics as Metrics
import qualified Journal.MP as Journal
import Journal.Types
       ( Journal
       , Options
       , Subscriber(..)
       , jMetadata
       , oLogger
       , oMaxSubscriber
       , oTermBufferLength
       , readBytesConsumed
       , writeBytesConsumed
       )
import Ltl.Json (mergePatch)
import Options.Generic

import Dumblog.Common.Metrics (dUMBLOG_METRICS, dumblogSchema)
import Dumblog.Journal.Blocker (emptyBlocker)
import Dumblog.Journal.Codec (Envelope(..), decode)
import Dumblog.Journal.FrontEnd (FrontEndInfo(..), runFrontEnd)
import qualified Dumblog.Journal.Logger as DLogger
import Dumblog.Journal.Snapshot (Snapshot)
import qualified Dumblog.Journal.Snapshot as Snapshot
import Dumblog.Journal.StateMachine
       (InMemoryDumblog, initState, runCommand)
import Dumblog.Journal.Types (Command(..))
import Dumblog.Journal.Worker (WorkerInfo(..), worker)

------------------------------------------------------------------------

-- TODO: we should just reads until we hit `before`
fetchJournal :: Maybe Snapshot -> FilePath -> Options -> IO Journal
fetchJournal mSnapshot fpj opts = do
  allocateJournal fpj opts
  journal <- startJournal fpj opts
  case mSnapshot of
    Nothing -> do
      before <- readBytesConsumed (jMetadata journal) Sub1
      putStrLn $ "[journal] Didn't Find a Snapshot! starting beginning to: " <> show before
      writeBytesConsumed (jMetadata journal) Sub1 0
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
  (s', _) <- runCommand DLogger.ioLogger s cmd
  replay cmds s'

type DebugFile = Vector InstanceStateRepr

-- TODO: merge with `replay`
replayDebug :: [Command] -> InMemoryDumblog -> IO DebugFile
replayDebug originCommands originState = do
  queueLogger <- DLogger.newQueueLogger
  go queueLogger 0 mempty originCommands originState
  where
    go _ _logTime dfile [] _s = do
      putStrLn "[REPLAY-DEBUG] finished!"
      pure dfile
    go logger logTime dfile (cmd:cmds) s = do
      putStrLn $ "[REPLAY-DEBUG] running: " <> show cmd
      (s', _) <- runCommand (DLogger.queueLogger logger) s cmd
      logLines <- DLogger.flushQueue logger
      let
        (ev, msg) = case cmd of
          Read i -> ("read", show i)
          Write logMsg -> ("write", Text.unpack (decodeUtf8 logMsg))
        ce = DebEvent
          { from = "client"
          , to = "dumblog"
          , event = ev
          , receivedLogical = logTime
          , message = msg
          }
        is = InstanceStateRepr
             { state = LText.unpack (LEncoding.decodeUtf8 (Aeson.encode (mergePatch (Aeson.toJSON s) (Aeson.toJSON s'))))
             , currentEvent = ce
             , logs = logLines
             , sent = []
             }
      go logger (succ logTime) (Vector.snoc dfile is) cmds s'

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
      let Envelope _key cmd _arrivalTime = decode entry
      cmds <- collectAll jour
      pure $ cmd : cmds

startingState :: Maybe Snapshot -> InMemoryDumblog
startingState Nothing = initState
startingState (Just snap) = Snapshot.ssState snap

data DumblogConfig
  = Run
    { quiet :: Bool <?> "Should we suppress program log messages"}
  | DebugFile
    { output :: FilePath <?> "Where to output the debug file"
    }
  deriving (Generic, Show)

instance ParseRecord DumblogConfig

quietRun :: DumblogConfig
quietRun = Run (Helpful True)

{-
Unclear how to:
* How to archive the journal
-}

dumblogOptions :: Options
dumblogOptions = defaultOptions
  { oLogger = Logger.nullLogger
  , oMaxSubscriber = Sub2
  , oTermBufferLength = 512 * 1024 * 1024
  }

dUMBLOG_JOURNAL :: FilePath
dUMBLOG_JOURNAL = "/tmp/dumblog.journal"

dUMBLOG_SNAPSHOT :: FilePath
dUMBLOG_SNAPSHOT = "/tmp/dumblog.snapshot"

journalDumblog :: DumblogConfig -> Int -> Int -> Maybe (MVar ()) -> IO ()
journalDumblog cfg _capacity port mReady = do
  let fpj = dUMBLOG_JOURNAL
      fpm = dUMBLOG_METRICS
      fps = dUMBLOG_SNAPSHOT
      untilSnapshot = 1000
  case cfg of
    Run q -> do
      mSnapshot <- Snapshot.readFile fps
      journal <- fetchJournal mSnapshot fpj dumblogOptions
      metrics <- Metrics.newMetrics dumblogSchema fpm
      blocker <- emptyBlocker 0 -- it is okay to start over
      cmds <- collectAll journal
      workerState <- replay cmds (startingState mSnapshot)
      let
        events = length cmds
        feInfo = FrontEndInfo blocker
        logger | unHelpful q = DLogger.nullLogger
               | otherwise = DLogger.ioLogger
        wInfo = WorkerInfo blocker logger fps events untilSnapshot
      withAsync (worker journal metrics wInfo workerState) $ \a -> do
        link a
        runFrontEnd port journal metrics feInfo mReady
    DebugFile fp -> do
      mSnapshot <- Snapshot.readFile fps
      journal <- fetchJournal mSnapshot fpj dumblogOptions
      cmds <- collectAll journal
      debugFileContents <- replayDebug cmds (startingState mSnapshot)
      Aeson.encodeFile (unHelpful fp) debugFileContents
      putStrLn "Generated Debug-file"
