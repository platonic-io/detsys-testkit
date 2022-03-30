{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}

module Dumblog.Journal.Main where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (link, withAsync)
import Control.Concurrent.MVar (MVar)
import Control.Exception (bracket_)
import qualified Data.Aeson as Aeson
import Data.Binary (decode)
import Data.Maybe (fromMaybe)
import Data.TreeDiff (ansiWlPretty, ediff, ppEditExpr)
import Data.Vector (Vector)
import qualified Data.Vector as Vector
import Debugger.State (DebEvent(..), InstanceStateRepr(..))
import Journal
       (allocateJournal, journalMetadata, startJournal)
import qualified Journal.Internal.Metrics as Metrics
import qualified Journal.MP as Journal
import Journal.Types
       ( Journal
       , Options
       , Subscriber(..)
       , activeTermCount
       , computeTermBeginPosition
       , indexByTermCount
       , jMetadata
       , positionBitsToShift
       , rawTailTermId
       , rawTailTermOffset
       , readBytesConsumed
       , readInitialTermId
       , readRawTail
       , readTermLength
       , writeBytesConsumed
       )
import Options.Generic
import System.Directory (copyFile, getTemporaryDirectory, removeFile)
import System.FilePath ((<.>), (</>))

import Dumblog.Common.Constants (dUMBLOG_PORT, dumblogOptions, dumblogJournalPath, dumblogSnapshotPath)
import Dumblog.Common.Metrics (dumblogSchema, dumblogMetricsPath)
import Dumblog.Journal.Blocker (emptyBlocker)
import Dumblog.Journal.Codec (Envelope(..))
import Dumblog.Journal.FrontEnd (FrontEndInfo(..), runFrontEnd)
import qualified Dumblog.Journal.Logger as DLogger
import Dumblog.Journal.Snapshot (Snapshot)
import qualified Dumblog.Journal.Snapshot as Snapshot
import Dumblog.Journal.StateMachine (InMemoryDumblog, initState)
import Dumblog.Journal.Types (Input(..), Output(..), CommandName(..))
import Dumblog.Journal.Versions (dUMBLOG_CURRENT_VERSION, runCommand)
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

replay :: [Envelope Input] -> InMemoryDumblog -> IO InMemoryDumblog
replay [] s = do
  putStrLn "[REPLAY] finished!"
  pure s
replay (env:cmds) s = do
  putStrLn $ "[REPLAY] running: " <> show (eContent env)
  (s', _) <- runCommand (eVersion env) DLogger.ioLogger s (eContent env)
  replay cmds s'

type DebugFile = Vector InstanceStateRepr

-- TODO: merge with `replay`
replayDebug :: [Envelope Input] -> InMemoryDumblog -> IO DebugFile
replayDebug originCommands originState = do
  queueLogger <- DLogger.newQueueLogger
  go queueLogger 0 mempty originCommands originState
  where
    go _ _logTime dfile [] _s = do
      putStrLn "[REPLAY-DEBUG] finished!"
      pure dfile
    go logger logTime dfile (env:cmds) s = do
      let cmd = eContent env
          v = eVersion env
      putStrLn $ "[REPLAY-DEBUG] running: " <> show cmd
      (s', r) <- runCommand v (DLogger.queueLogger logger) s cmd
      logLines <- DLogger.flushQueue logger
      let
        msg = show cmd
        ce = DebEvent
          { from = case cmd of
              ClientRequest {} -> "client"
              InternalMessageIn {} -> "backup"
              AdminCommand {} -> "admin"
          , to = "dumblog"
          , event = commandName cmd
          , receivedLogical = logTime
          , message = msg
          }
        is = InstanceStateRepr
             { state = show (ppEditExpr ansiWlPretty (ediff s s'))
             , currentEvent = ce
             , runningVersion = v
             , receivedTime = eArrival env
             , logs = logLines
             , sent = [ DebEvent
                        { from = "dumblog"
                        , to = case r of
                            ClientResponse {} -> "client"
                            InternalMessageOut {} -> "backup"
                            AdminResponse {} -> "admin"
                        , event = commandName r
                        , receivedLogical = logTime
                        , message = show r
                        }
                      ]
             }
      go logger (succ logTime) (Vector.snoc dfile is) cmds s'

collectAll :: Journal -> IO [Envelope Input]
collectAll jour = do
  putStrLn "[collect] Checking journal for old-entries"
  val <- Journal.readLazyJournal jour Sub1
  case val of
    Nothing -> do
      putStrLn "[collect] No more entries"
      pure []
    Just entry -> do
      putStrLn "[collect] Found an entry"
      let env = decode entry
      cmds <- collectAll jour
      pure $ env : cmds

startingState :: Maybe Snapshot -> InMemoryDumblog
startingState Nothing     = initState
startingState (Just snap) = Snapshot.ssState snap

data DumblogConfig
  = Run
    { quiet :: Bool <?> "Should we suppress program log messages"
    , port  :: Maybe Int
    }
  | DebugFile
    { output :: FilePath <?> "Where to output the debug file"
    }
  | DebugFileWatch
    { output :: FilePath <?> "Where to output the debug file"
    }
  deriving (Generic, Show)

instance ParseRecord DumblogConfig

quietRun :: DumblogConfig
quietRun = Run (Helpful True) Nothing

{-
Unclear how to:
* How to archive the journal
-}

journalDumblog :: DumblogConfig -> Int -> Maybe (MVar ()) -> IO ()
journalDumblog cfg _capacity mReady = do
  case cfg of
    Run q mPort -> do
      let portToUse = fromMaybe dUMBLOG_PORT mPort
      mSnapshot <- Snapshot.readFile (dumblogSnapshotPath portToUse)
      journal <- fetchJournal mSnapshot (dumblogJournalPath portToUse) dumblogOptions
      metrics <- Metrics.newMetrics dumblogSchema (dumblogMetricsPath portToUse)
      blocker <- emptyBlocker 0 -- it is okay to start over
      cmds <- collectAll journal
      workerState <- replay cmds (startingState mSnapshot)
      let
        events = length cmds
        feInfo = FrontEndInfo blocker dUMBLOG_CURRENT_VERSION
        logger | unHelpful q = DLogger.nullLogger
               | otherwise = DLogger.ioLogger
        untilSnapshot = 1000
        wInfo = WorkerInfo blocker logger (dumblogSnapshotPath portToUse)
                           dUMBLOG_CURRENT_VERSION events untilSnapshot
      withAsync (worker journal metrics wInfo workerState) $ \a -> do
        link a
        runFrontEnd portToUse journal metrics feInfo mReady
    DebugFile debugFile -> genDebugFile (dumblogJournalPath dUMBLOG_PORT) -- XXX: port is hardcoded.
                                        (dumblogSnapshotPath dUMBLOG_PORT)
                                        (unHelpful debugFile)
    DebugFileWatch debugFile -> do
      putStrLn "[journal]: waiting for journal changes..."
      watch (dumblogJournalPath dUMBLOG_PORT) (dumblogSnapshotPath dUMBLOG_PORT) (unHelpful debugFile)

watch :: FilePath -> FilePath -> FilePath -> IO ()
watch journalFile snapshotFile debugFile = go 0
  where
    go lastBytesProduced = do
      eMeta <- journalMetadata journalFile dumblogOptions
      case eMeta of
        Left err -> do
          putStrLn ("[watch] error: " ++ show err)
          threadDelay 10000
          go lastBytesProduced
        Right meta -> do
          bytesProduced <- readBytesProduced meta
          if bytesProduced /= lastBytesProduced
          then do
            putStrLn "[watch] journal has changed!"
            genDebugFile journalFile snapshotFile debugFile
            go bytesProduced
          else threadDelay 10000 >> go lastBytesProduced

    readBytesProduced meta = do
      termCount <- activeTermCount meta
      let index = indexByTermCount termCount
      rt <- readRawTail meta index
      initTermId <- readInitialTermId meta
      termLen <- readTermLength meta
      if termLen == 0
      then return 0
      else do
        let termId            = rawTailTermId rt
            termOffset        = rawTailTermOffset rt termLen
            termBeginPosition =
              computeTermBeginPosition termId (positionBitsToShift termLen) initTermId

            produced = termBeginPosition + fromIntegral termOffset
        return produced

genDebugFile :: FilePath -> FilePath -> FilePath -> IO ()
genDebugFile journalFile snapshotFile debugFile =
  withTempCopy journalFile $ \journalCopy -> do
    mSnapshot <- Snapshot.readFile snapshotFile
    journal <- fetchJournal mSnapshot journalCopy dumblogOptions
    cmds <- collectAll journal
    debugFileContents <- replayDebug cmds (startingState mSnapshot)
    Aeson.encodeFile debugFile debugFileContents
    putStrLn "Generated Debug-file"

withTempCopy :: FilePath -> (FilePath -> IO a) -> IO a
withTempCopy fp k = do
  tmpDir <- getTemporaryDirectory
  let tmpFile = tmpDir </> fp <.> "tmp"
  bracket_ (copyFile fp tmpFile) (removeFile tmpFile) (k tmpFile)
