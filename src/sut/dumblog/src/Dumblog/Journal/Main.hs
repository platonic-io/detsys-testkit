{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}

module Dumblog.Journal.Main where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (link, withAsync)
import Control.Concurrent.MVar (MVar)
import Control.Exception (bracket_)
import qualified Data.Aeson as Aeson
import Data.Int (Int64)
import Data.TreeDiff (ansiWlPretty, ediff, ppEditExpr)
import Data.Vector (Vector)
import qualified Data.Vector as Vector
import Debugger.State (DebEvent(..), InstanceStateRepr(..))
import Journal
       (allocateJournal, defaultOptions, journalMetadata, startJournal)
import Journal.Internal.Logger as Logger
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
       , oLogger
       , oMaxSubscriber
       , oTermBufferLength
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
import Data.Binary (decode)
import System.Directory (copyFile, getTemporaryDirectory, removeFile)
import System.FilePath ((<.>), (</>))

import Dumblog.Common.Metrics (dUMBLOG_METRICS, dumblogSchema)
import Dumblog.Journal.Blocker (emptyBlocker)
import Dumblog.Journal.Codec (Envelope(..))
import Dumblog.Journal.FrontEnd (FrontEndInfo(..), runFrontEnd)
import qualified Dumblog.Journal.Logger as DLogger
import Dumblog.Journal.Snapshot (Snapshot)
import qualified Dumblog.Journal.Snapshot as Snapshot
import Dumblog.Journal.StateMachine
       (InMemoryDumblog, initState)
import Dumblog.Journal.Types (Input(..), ClientRequest(..))
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

replay :: [(Int64, Input)] -> InMemoryDumblog -> IO InMemoryDumblog
replay [] s = do
  putStrLn "[REPLAY] finished!"
  pure s
replay ((v, cmd):cmds) s = do
  putStrLn $ "[REPLAY] running: " <> show cmd
  (s', _) <- runCommand v DLogger.ioLogger s cmd
  replay cmds s'

type DebugFile = Vector InstanceStateRepr

-- TODO: merge with `replay`
replayDebug :: [(Int64, Input)] -> InMemoryDumblog -> IO DebugFile
replayDebug originCommands originState = do
  queueLogger <- DLogger.newQueueLogger
  go queueLogger 0 mempty originCommands originState
  where
    go _ _logTime dfile [] _s = do
      putStrLn "[REPLAY-DEBUG] finished!"
      pure dfile
    go logger logTime dfile ((v, cmd):cmds) s = do
      putStrLn $ "[REPLAY-DEBUG] running: " <> show cmd
      (s', r) <- runCommand v (DLogger.queueLogger logger) s cmd
      logLines <- DLogger.flushQueue logger
      let
        ev = case cmd of
          ClientRequest (Read {}) -> "read"
          ClientRequest (Write {})-> "write"
        msg = show cmd
        ce = DebEvent
          { from = "client"
          , to = "dumblog"
          , event = ev
          , receivedLogical = logTime
          , message = msg
          }
        is = InstanceStateRepr
             { state = show (ppEditExpr ansiWlPretty (ediff s s'))
             , currentEvent = ce
             , runningVersion = v
             , logs = logLines
             , sent = [ DebEvent
                        { from = "dumblog"
                        , to = "client"
                        , event = ev
                        , receivedLogical = logTime
                        , message = show r
                        }
                      ]
             }
      go logger (succ logTime) (Vector.snoc dfile is) cmds s'

collectAll :: Journal -> IO [(Int64, Input)]
collectAll jour = do
  putStrLn "[collect] Checking journal for old-entries"
  val <- Journal.readLazyJournal jour Sub1
  case val of
    Nothing -> do
      putStrLn "[collect] No more entries"
      pure []
    Just entry -> do
      putStrLn "[collect] Found an entry"
      let Envelope _key cmd version _arrivalTime = decode entry
      cmds <- collectAll jour
      pure $ (version, cmd) : cmds

startingState :: Maybe Snapshot -> InMemoryDumblog
startingState Nothing     = initState
startingState (Just snap) = Snapshot.ssState snap

data DumblogConfig
  = Run
    { quiet :: Bool <?> "Should we suppress program log messages" }
  | DebugFile
    { output :: FilePath <?> "Where to output the debug file"
    }
  | DebugFileWatch
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
        feInfo = FrontEndInfo blocker dUMBLOG_CURRENT_VERSION
        logger | unHelpful q = DLogger.nullLogger
               | otherwise = DLogger.ioLogger
        wInfo = WorkerInfo blocker logger fps dUMBLOG_CURRENT_VERSION events untilSnapshot
      withAsync (worker journal metrics wInfo workerState) $ \a -> do
        link a
        runFrontEnd port journal metrics feInfo mReady
    DebugFile fp -> debugFile (unHelpful fp)
    DebugFileWatch fp -> do
      putStrLn "[journal]: waiting for journal changes..."
      watch (unHelpful fp)

watch :: FilePath -> IO ()
watch fp = go 0
  where
    go lastBytesProduced = do
      eMeta <- journalMetadata dUMBLOG_JOURNAL dumblogOptions
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
            debugFile fp
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

debugFile :: FilePath -> IO ()
debugFile fp = withTempCopy dUMBLOG_JOURNAL $ \fpjCopy -> do
  mSnapshot <- Snapshot.readFile dUMBLOG_SNAPSHOT
  journal <- fetchJournal mSnapshot fpjCopy dumblogOptions
  cmds <- collectAll journal
  debugFileContents <- replayDebug cmds (startingState mSnapshot)
  Aeson.encodeFile fp debugFileContents
  putStrLn "Generated Debug-file"

withTempCopy :: FilePath -> (FilePath -> IO a) -> IO a
withTempCopy fp k = do
  tmpDir <- getTemporaryDirectory
  let tmpFile = tmpDir </> fp <.> "tmp"
  bracket_ (copyFile fp tmpFile) (removeFile tmpFile) (k tmpFile)
