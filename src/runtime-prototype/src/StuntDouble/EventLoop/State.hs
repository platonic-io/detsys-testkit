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
  , loopStateActorState :: TVar (Map LocalRef State)
  , loopStateIOAsyncs :: TVar [Async IOResult]
  , loopStateIOContinuations :: TVar (Map (Async IOResult)
                                          (RemoteRef, CorrelationId, IOResult -> Actor))
  , loopStateTransport :: Transport IO -- Will not change once created, so doesn't need STM?
  , loopStateNextCorrelationId :: TVar CorrelationId
  , loopStateResponses     :: TVar (Map CorrelationId (TMVar Message))
  , loopStateWaitingAsyncs :: TVar (Map CorrelationId (Async Message))
  , loopStateContinuations :: TVar (Map (Async Message)
                                        (RemoteRef, CorrelationId, Message -> Actor))
  , loopStateEventLog :: TVar EventLog
  }

lookupActor :: LocalRef -> TVar (Map LocalRef (Message -> Actor))
            -> IO (Maybe (Message -> Actor))
lookupActor key var = Map.lookup key <$> atomically (readTVar var)

installContinuation :: Async Message -> RemoteRef -> CorrelationId
                    -> (Message -> Actor) -> LoopState -> IO ()
installContinuation a self corrId k ls = atomically $
  modifyTVar' (loopStateContinuations ls) (Map.insert a (self, corrId, k))

recallContinuation :: Async Message -> LoopState
                   -> IO (Maybe (RemoteRef, CorrelationId, Message -> Actor))
recallContinuation a ls = do
  ks <- atomically $ do
    ks <- readTVar (loopStateContinuations ls)
    writeTVar (loopStateContinuations ls) (Map.delete a ks)
    return ks
  return (Map.lookup a ks)

installIOContinuation :: Async IOResult -> RemoteRef -> CorrelationId
                      -> (IOResult -> Actor) -> LoopState -> IO ()
installIOContinuation a self corrId k ls = atomically $
    modifyTVar' (loopStateIOContinuations ls) (Map.insert a (self, corrId, k))

recallIOContinuation :: Async IOResult -> LoopState
                     -> IO (RemoteRef, CorrelationId, IOResult -> Actor)
recallIOContinuation a ls = do
  ks <- atomically $ do
    ks <- readTVar (loopStateIOContinuations ls)
    writeTVar (loopStateIOContinuations ls) (Map.delete a ks)
    return ks
  return (ks Map.! a)

correlateAsync :: CorrelationId -> Async Message -> LoopState -> IO ()
correlateAsync cid a ls = atomically $
  modifyTVar' (loopStateWaitingAsyncs ls) (Map.insert cid a)

say :: LoopState -> String -> IO ()
say ls s = emit ls (LogComment s)

saySTM :: LoopState -> String -> STM ()
saySTM ls s = emitSTM ls (LogComment s)

dumpState :: LoopState -> IO ()
dumpState ls = do
  putStrLn ""
  putStrLn "=== LOOPSTATE DUMP ==="
  putStr "loopStateName = "
  putStrLn (getEventLoopName (loopStateName ls))
  putStr "loopStateActorState = "
  states <- readTVarIO (loopStateActorState ls)
  print states
  corrId <- readTVarIO (loopStateNextCorrelationId ls)
  putStr "loopStateNextCorrelationId = "
  print corrId
  putStr "loopStateResponses.keys = "
  responses <- readTVarIO (loopStateResponses ls)
  print (Map.keys responses)
  putStr "loopStateWaitingAsyncs.keys = "
  asyncs <- readTVarIO (loopStateWaitingAsyncs ls)
  print (Map.keys asyncs)
  putStr "loopStateContinuations.keys.length = "
  conts <- readTVarIO (loopStateContinuations ls)
  print (length (Map.keys conts))
  putStr "loopStateQueue = ["
  events <- atomically (flushTBQueue (loopStateQueue ls))
  mapM_ (\e -> putStrLn (eventName e ++ ",")) events
  putStrLn "]"
  putStrLn "=== END OF LOOPSTATE DUMP ==="

emitSTM :: LoopState -> (EventLoopName -> LogEntry) -> STM ()
emitSTM ls e = modifyTVar' (loopStateEventLog ls) (e (loopStateName ls) :)

emit :: LoopState -> (EventLoopName -> LogEntry) -> IO ()
emit ls e = atomically (emitSTM ls e)
