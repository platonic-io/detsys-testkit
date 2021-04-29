{-# language CPP, DataKinds, DeriveGeneric, OverloadedStrings, RankNTypes, RecordWildCards, TemplateHaskell, TypeOperators #-}
module Main where

import Control.Monad.Trans.State
import Control.Lens (Lens', set, (^.))
import Control.Lens.TH (makeLenses)
import Data.Map (Map)
import qualified Data.Map as Map
import GHC.Stack
import Options.Generic

import Scheduler
import Scheduler.Event(Event(Event))
import qualified Scheduler.Json as Json
import Scheduler.Time (Timestamp)
import Scheduler.Types
import qualified Scheduler.Agenda as Agenda
import qualified Scheduler.Client as Client
import qualified Scheduler.Executor as Executor
import qualified Scheduler.Random as Random
import qualified Scheduler.Time as Time
import qualified Scheduler.Trace as Trace

#ifdef __BAZEL_BUILD__
import GitHash
-- When building with cabal we expect the git commit hash to be passed in via
-- CPP flags, i.e. `--ghc-option=-D__GIT_HASH__=\"X\"`.
#elif defined __GIT_HASH__
gitHash :: String
gitHash = __GIT_HASH__
#else
gitHash :: String
gitHash = "unknown"
#endif

data Config
  = Run
    { testId :: Int <?> "Which TestId to use",
      runId :: Int <?> "Which RunId to use"
    }
  | Version
  | Help
  deriving Generic

instance ParseRecord Config

todo_ :: HasCallStack => a
todo_ = withFrozenCallStack (error "Not Implemented")

type Ref = String

data SState = SState
  { _agenda :: Agenda.Agenda
  , _activeClients :: Map String Timestamp
  , _time :: Time.State
  }

makeLenses ''SState

type MIO = StateT SState IO

runMIO :: SState -> MIO a -> IO a
runMIO = flip evalStateT

modifyMIO :: Lens' SState a -> (a -> (a, b)) -> MIO b
modifyMIO l f = do
  ss <- get
  let (sl, res) = f (ss ^. l)
  put $ set l sl ss
  return res

mkAgenda :: IO (Agenda.Capability MIO)
mkAgenda = do
  return $ Agenda.fromModify $ modifyMIO agenda

mkClient :: IO (Client.Capability MIO)
mkClient = do
  return $ Client.Capability
    { Client.isClient = return . Client.colonClient
    , Client.isClientActive = \ c -> do
        cm <- gets _activeClients
        return $ Map.lookup c cm
    , Client.activateClient = todo_
    , Client.deActivateClient = todo_
    }

mkRandom :: IO (Random.Capability MIO)
mkRandom = do
  return $ Random.Capability
    { Random.gen = \n ->
        if n == 0 then return [] else todo_}

mkTrace :: IO (Trace.Capability MIO)
mkTrace = do
  return $ Trace.Capability
    { Trace.emitEvent = \t r nt -> return () -- TODO
    }

mkExecutor :: IO (Executor.Capability Ref MIO)
mkExecutor = do
  return $ Executor.Capability
    { Executor.execute = \r s -> return [] -- TODO
    , Executor.timer = todo_
    }

mkTime :: IO (Time.Capability MIO)
mkTime = do
  return $ Time.fromModify $ modifyMIO time

mkRunInfo :: IO (RunInfo Ref)
mkRunInfo = return $ RunInfo
  { totalExecutors = todo_
  , faults = []
  , topology = todo_
  , minTimeNs = todo_
  , maxTimeNs = todo_
  , clientTimeout = todo_
  , clientDelay = todo_
  }

main :: IO ()
main = do
  (cfg, help) <- getWithHelp "Scheduler"
  case cfg of
    Version -> putStrLn gitHash
    Help -> help
    Run {..} -> do
      runInfo <- mkRunInfo
      agendaC <- mkAgenda
      clientC <- mkClient
      randomC <- mkRandom
      traceC <- mkTrace
      executorC <- mkExecutor
      timeC <- mkTime
      let s = SState {
            _agenda = Agenda.empty,
            _activeClients = Map.empty,
            _time = Time.empty
            }
          entries = [Agenda.mkEntry 0 $ Event "invoke" "write" Json.null "frontend" "client:0" Nothing]
      runMIO s $ do
        Agenda.addEntries agendaC entries
        run
          (TestId $ unHelpful $ testId)
          (RunId $ unHelpful $ runId)
          runInfo agendaC clientC randomC traceC executorC timeC
