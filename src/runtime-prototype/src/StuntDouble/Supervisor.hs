module StuntDouble.Supervisor where

import System.Random
import Data.Maybe
import Control.Exception
import Control.Concurrent
import Control.Concurrent.Async

import StuntDouble.Actor

------------------------------------------------------------------------

data Supervisor = Supervisor
  { supervisorRestartStrategy     :: RestartStrategy
  , supervisorMaxRestartIntensity :: MaxRestartIntensity
  , supervisorChildren            :: [ChildSpec]
  }

data RestartStrategy
  = OneForOne -- | If one child process terminates and should be restarted, only
              -- that child process is affected.

  | OneForAll -- | If one child process terminates and should be restarted, all
              -- other child processes are terminated and then all child
              -- processes are restarted.


  | RestForOne -- | If one child process terminates and should be restarted, the
               -- 'rest' of the child processes -- i.e. the child processes
               -- after the terminated child process in the start order -- are
               -- terminated. Then the terminated child process and all child
               -- processes after it are restarted.

defaultRestartStrategy :: RestartStrategy
defaultRestartStrategy = OneForOne

-- | To prevent a supervisor from getting into an infinite loop of child process
-- terminations and restarts, a maximum restart intensity is defined using two
-- integer values specified by the fields `intensity` and `period`.
--
-- | If more than `intensity` restarts occur within `period` seconds, the
-- supervisor terminates all child processes and then itself. The termination
-- reason for the supervisor itself in that case will be shutdown.
data MaxRestartIntensity = MaxRestartIntensity
  { intensity :: Int
  , period    :: Int
  }

defaultMaxRestartIntensity :: MaxRestartIntensity
defaultMaxRestartIntensity = MaxRestartIntensity 1 5

defaultSupervisor :: [ChildSpec] -> Supervisor
defaultSupervisor = Supervisor defaultRestartStrategy defaultMaxRestartIntensity

data ChildSpec = ChildSpec
  { childChildId   :: ChildId
  , childRestart   :: Restart
  , childShutdown  :: Shutdown
  , childChildType :: ChildType
  }

newtype ChildId = ChildId { getChildId :: String }
  deriving Eq

data Restart
  = Permanent -- | Child process will always be restarted.
  | Temporary -- | Child process will never be restarted.
  | Transient -- | Child process will only be restarted if terminated
              -- abnormally, i.e. another exit reason than normal or shutdown.

defaultRestart :: Restart
defaultRestart = Permanent

-- | How long to wait between graceful and forceful shutdown.
data Shutdown
  = Now         -- | Immediately do a forceful shutdown.
  | Timeout Int -- | In milliseconds
  | Infinity    -- | Wait forever.

defaultWorkerShutdown :: Shutdown
defaultWorkerShutdown = Timeout 5000

defaultSupervisorShutdown :: Shutdown
defaultSupervisorShutdown = Infinity

data ChildType
  = WorkerChild Service
  | SupervisorChild Supervisor

newtype ServiceRef = ServiceRef { getServiceRef :: Async () }

data Service = Service
  { start  :: IO ServiceRef
  , stop   :: ServiceRef -> IO ()
  , status :: ServiceRef -> IO (Maybe (Either SomeException ()))
  }

defaultWorkerChild :: ChildId -> Service -> ChildSpec
defaultWorkerChild childId service =
  ChildSpec childId defaultRestart defaultWorkerShutdown (WorkerChild service)

defaultSupervisorChild :: ChildId -> Supervisor -> ChildSpec
defaultSupervisorChild childId supervisor =
  ChildSpec childId defaultRestart defaultSupervisorShutdown (SupervisorChild supervisor)

------------------------------------------------------------------------

type UTCTime = Int -- XXX

tooManyRestarts :: [UTCTime] -> MaxRestartIntensity -> ([UTCTime], Bool)
tooManyRestarts [] _ = ([], False)
tooManyRestarts ts (MaxRestartIntensity i p) = (ts, False) -- XXX:

-- An abnormal exit reason than normal or shutdown.
isAbnormal :: SomeException -> Bool
isAbnormal e
  | fromException e == Just AsyncCancelled = False -- Shutdown from supervisor.
  | otherwise                              = True

lookupRestart :: ChildId -> [ChildSpec] -> Maybe Restart
lookupRestart childId
  = fmap childRestart . listToMaybe . filter ((== childId) . childChildId)

------------------------------------------------------------------------

exampleWorker :: String -> Service
exampleWorker name = Service
  { start = ServiceRef <$> (async $ do
      putStrLn ("Hello, I'm " ++ name)
      secs <- randomRIO (1, 3)
      threadDelay (secs * 1000000)
      b <- randomIO
      if b then return () else error "I quit!")
  , stop   = cancel . getServiceRef
  , status = poll . getServiceRef
  }

exampleSupervisor :: Supervisor
exampleSupervisor = defaultSupervisor
  [ defaultWorkerChild (ChildId "A") (exampleWorker "A")
  , defaultWorkerChild (ChildId "B") (exampleWorker "B") ]

------------------------------------------------------------------------

spawnSupervisor :: Supervisor -> IO ()
spawnSupervisor sup = do
  let children = supervisorChildren sup
      cids     = map childChildId children
      cts      = map childChildType children
  ws <- mapM spawnChildren cts
  go (zip3 cids cts ws) []
  where
    go :: [(ChildId, ChildType, ServiceRef)] -> [UTCTime] -> IO ()
    go (wref@(cid, ct, ref) : wrefs) restarts = do
      threadDelay 10000
      s <- status (getService ct) ref
      case s of
        Nothing -> go (wrefs ++ [wref]) restarts
        Just (Left exception) -> do
          putStrLn (getChildId cid ++ " exited with ``" ++ show exception ++ "''")
          let (restarts', yes) =
                tooManyRestarts restarts (supervisorMaxRestartIntensity sup)
          if yes
          then
            mapM_ (\(cid, ct, ref) -> stop (getService ct) ref) wrefs
            -- XXX: throw Shutdown
          else do
            case lookupRestart cid (supervisorChildren sup) of
              Just Permanent ->
                case supervisorRestartStrategy sup of
                  OneForOne -> do
                    ref' <- start (getService ct)
                    go (wrefs ++ [(cid, ct, ref')]) restarts'
                  OneForAll -> do
                    undefined
                  RestForOne -> do
                    -- We need the original order here
                    undefined

              Just Temporary -> go wrefs restarts'
              Just Transient | isAbnormal exception -> do
                                 ref' <- start (getService ct)
                                 go (wrefs ++ [(cid, ct, ref')]) restarts'
                             | otherwise -> go wrefs restarts'
              Nothing -> error "spawnSupervisor: impossible"

        Just (Right _) ->
          case lookupRestart cid (supervisorChildren sup) of
            Just Permanent -> do
              ref' <- start (getService ct)
              go (wrefs ++ [(cid, ct, ref')]) restarts
            Just Temporary -> go wrefs restarts
            Just Transient -> go wrefs restarts
            Nothing -> error "spawnSupervisor: impossible"

spawnSupervisor' :: Supervisor -> IO ServiceRef
spawnSupervisor' = fmap ServiceRef . async . spawnSupervisor

supervisorService :: Supervisor -> Service
supervisorService sup = Service
  { start  = spawnSupervisor' sup
  , stop   = cancel . getServiceRef
  , status = poll . getServiceRef
  }

getService :: ChildType -> Service
getService (WorkerChild w)       = w
getService (SupervisorChild sup) = supervisorService sup

spawnChildren :: ChildType -> IO ServiceRef
spawnChildren (WorkerChild worker)  = start worker
spawnChildren (SupervisorChild sup) = spawnSupervisor' sup

test :: IO ()
test = spawnSupervisor exampleSupervisor
