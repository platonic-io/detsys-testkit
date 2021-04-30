module StuntDouble.EventLoop.State where

import Data.Map (Map)
import qualified Data.Map as Map
import Control.Concurrent.STM
import Control.Concurrent.Async

import StuntDouble.EventLoop.Event
import StuntDouble.EventLoop.Transport
import StuntDouble.Actor
import StuntDouble.Reference
import StuntDouble.Message

------------------------------------------------------------------------

data LoopState = LoopState
  { loopStateName :: EventLoopName
  , loopStatePids :: TVar [Async ()] -- | Holds the `Async`s (or PIDs) of the
                                     --   event loop itself.
  , loopStateQueue :: TBQueue Event
  , loopStateActors :: TVar (Map LocalRef (Message -> Actor)) -- XXX: Only changed by main loop, so no need for STM?
  , loopStateIOHandlers :: TVar (Map (Async IOResult) (LocalRef, IOResult -> Actor))
  , loopStateIOAsyncs :: TVar [Async IOResult]
  , loopStateTransport :: Transport IO -- Will not change once created, so doesn't need STM?
  , loopStateNextCorrelationId :: TVar CorrelationId
  , loopStateResponses :: TVar (Map CorrelationId (TMVar Message))
  , loopStateWaitingAsyncs :: TVar (Map CorrelationId (Async Message))
  , loopStateContinuations :: TVar (Map (Async Message) (Message -> Actor))
  , loopStateLogs :: TVar [String]
  }

dummyDeveloperRef :: LoopState -> RemoteRef
dummyDeveloperRef ls = RemoteRef (getEventLoopName (loopStateName ls)) dummyIndex
  where
    dummyIndex = -1

lookupActor :: LocalRef -> TVar (Map LocalRef (Message -> Actor))
            -> IO (Maybe (Message -> Actor))
lookupActor key var = Map.lookup key <$> atomically (readTVar var)

installContinuation :: Async Message -> (Message -> Actor) -> LoopState -> IO ()
installContinuation a k ls = atomically $
  modifyTVar' (loopStateContinuations ls) (Map.insert a k)

recallContinuation :: Async Message -> LoopState -> IO (Message -> Actor)
recallContinuation a ls = do
  ks <- atomically $ do
    ks <- readTVar (loopStateContinuations ls)
    writeTVar (loopStateContinuations ls) (Map.delete a ks)
    return ks
  return (ks Map.! a)

correlateAsync :: CorrelationId -> Async Message -> LoopState -> IO ()
correlateAsync cid a ls = atomically $
  modifyTVar' (loopStateWaitingAsyncs ls) (Map.insert cid a)

say' :: TVar [String] -> String -> IO ()
say' logs s = atomically (modifyTVar' logs (s :))

say :: LoopState -> String -> IO ()
say ls s = do
  let name = getEventLoopName (loopStateName ls)
  atomically (modifyTVar' (loopStateLogs ls) ((name ++ "> " ++ s) :))

displayLogs :: LoopState -> IO ()
displayLogs ls = do
  logs <- readTVarIO (loopStateLogs ls)
  mapM_ putStrLn (reverse logs)

displayLogs' :: TVar [String] -> IO ()
displayLogs' logsVar = do
  logs <- readTVarIO logsVar
  mapM_ putStrLn (reverse logs)
