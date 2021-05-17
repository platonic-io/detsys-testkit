{-# LANGUAGE ScopedTypeVariables #-}

module StuntDouble.EventLoop where

import Data.Map (Map)
import qualified Data.Map as Map
import Control.Exception
import Control.Concurrent
import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Concurrent.STM.TBQueue
import System.Timeout

import StuntDouble.Actor
import StuntDouble.Actor.State
import StuntDouble.Message
import StuntDouble.Reference
import StuntDouble.EventLoop.Transport
import StuntDouble.FreeMonad
import StuntDouble.EventLoop.State
import StuntDouble.EventLoop.Event
import StuntDouble.EventLoop.InboundHandler
import StuntDouble.EventLoop.AsyncIOHandler

------------------------------------------------------------------------

newtype EventLoopRef = EventLoopRef
  { loopRefLoopState :: LoopState }

eventLog :: EventLoopRef -> IO EventLog
eventLog = fmap reverse . readTVarIO . loopStateEventLog . loopRefLoopState

emptyEventLog :: IO (TVar EventLog)
emptyEventLog = newTVarIO []

dump :: EventLoopRef -> IO ()
dump = dumpState . loopRefLoopState

dummyDeveloperRef :: EventLoopRef -> RemoteRef
dummyDeveloperRef r = dummyDeveloperRef' (loopRefLoopState r)

dummyDeveloperRef' :: LoopState -> RemoteRef
dummyDeveloperRef' ls = RemoteRef (getEventLoopName (loopStateName ls)) dummyIndex
  where
    dummyIndex = -1

------------------------------------------------------------------------

initLoopState :: EventLoopName -> Transport IO -> TVar EventLog -> IO LoopState
initLoopState name transport elog =
  LoopState
    <$> pure name
    <*> newTVarIO []
    <*> newTBQueueIO 128
    <*> newTVarIO Map.empty
    <*> newTVarIO Map.empty
    <*> newTVarIO []
    <*> newTVarIO Map.empty
    <*> pure transport
    <*> newTVarIO 0
    <*> newTVarIO Map.empty
    <*> newTVarIO Map.empty
    <*> newTVarIO Map.empty
    <*> pure elog

makeEventLoop :: FilePath -> EventLoopName -> TVar EventLog -> IO EventLoopRef
makeEventLoop fp name elog = do
  transport <- namedPipeTransport fp name
  ls <- initLoopState name transport elog
  aInHandler <- async (handleInbound ls)
  aAsyncIOHandler <- async (handleAsyncIO ls)
  aEvHandler <- async (handleEvents ls)
  atomically (modifyTVar' (loopStatePids ls)
               ([aInHandler, aEvHandler, aAsyncIOHandler] ++))
  return (EventLoopRef ls)

handleEvents :: LoopState -> IO ()
handleEvents ls = go
  where
    go = do
      e <- atomically (readTBQueue (loopStateQueue ls))
      say ls ("handleEvents: " ++ eventName e)
      handleEvent e ls
        `catch` \(exception :: SomeException) ->
                  say ls ("handleEvents: exception: " ++ show exception)
      go

handleEvent :: Event -> LoopState -> IO ()
handleEvent (Command c)  ls = handleCommand c ls
handleEvent (Receive r)  ls = handleReceive r ls
handleEvent (AsyncIODone a r) ls = handleAsyncIODone a r ls

handleCommand :: Command -> LoopState -> IO ()
handleCommand (Spawn actor respVar) ls = atomically $ do
  actors <- readTVar (loopStateActors ls)
  let lref = LocalRef (Map.size actors)
  writeTVar (loopStateActors ls) (Map.insert lref actor actors)
  modifyTVar' (loopStateActorState ls) (Map.insert lref initState)
  putTMVar respVar lref
handleCommand Quit ls = do
  pids <- atomically (readTVar (loopStatePids ls))
  threadDelay 100000
  mapM_ cancel pids

data ActorNotFound = ActorNotFound RemoteRef
  deriving Show

instance Exception ActorNotFound where

handleReceive :: Envelope -> LoopState -> IO ()
handleReceive e ls = case envelopeKind e of
  RequestKind -> do
    say ls ("handleReceive: Request: " ++ show e)
    mActor <- lookupActor (remoteToLocalRef (envelopeReceiver e)) (loopStateActors ls)
    case mActor of
      -- XXX: Throw here or just log and continue? Or take care of this at one
      -- layer above transport where we authenticate and verify messages are only
      -- going to known actors, i.e. that the remote refs are valid.
      Nothing -> throwIO (ActorNotFound (envelopeReceiver e))
      Just actor -> do
        cont <- runActor ls (envelopeReceiver e) (actor (envelopeMessage e))
        case cont of
          Now replyMsg -> do
            emit ls (LogRequest (envelopeSender e) (envelopeReceiver e) (envelopeMessage e)
                     replyMsg)
            say ls (show (replyEnvelope e replyMsg))
            transportSend (loopStateTransport ls) (replyEnvelope e replyMsg)
          Later async k -> do
            -- The actor has to talk to other remote actors before being able to reply.

            -- We need to save the correlation id of the original request, so
            -- that we can write the response to it once we resume our
            -- continuation.
            say ls "installing continuation"
            installContinuation async (envelopeReceiver e) (envelopeCorrelationId e) k ls
          LaterIO async k -> do
            say ls "installing i/o continuation"
            installIOContinuation async (envelopeReceiver e) (envelopeCorrelationId e) k ls

  ResponseKind -> do
    emit ls (LogReceive (envelopeSender e) (envelopeReceiver e) (envelopeMessage e)
              (envelopeCorrelationId e))
    resps <- readTVarIO (loopStateResponses ls)
    let respVar = resps Map.! envelopeCorrelationId e
    asyncs <- readTVarIO (loopStateWaitingAsyncs ls)
    let async = asyncs Map.! envelopeCorrelationId e
    mResumption <- recallContinuation async ls
    case mResumption of
      Nothing -> do
        emit ls (LogSendFinish (envelopeCorrelationId e) (envelopeMessage e))
        atomically (putTMVar respVar (envelopeMessage e))
      Just (self, corrId', k) -> do
        emit ls (LogComment (show (envelopeCorrelationId e)))
        cont <- runActor ls self (k (envelopeMessage e))
        case cont of
          Now replyMsg -> do
            emit ls (LogSendFinish (envelopeCorrelationId e) replyMsg)
            let respVar' = resps Map.! corrId'
            atomically (putTMVar respVar' replyMsg)
          Later {} -> do
            undefined

handleAsyncIODone :: Async IOResult -> IOResult -> LoopState -> IO ()
handleAsyncIODone a r ls = do
  (self, corrId, k) <- recallIOContinuation a ls
  emit ls (LogAsyncIOFinish corrId r)
  cont <- runActor ls self (k r)
  case cont of
    Now replyMsg -> do
      resps <- readTVarIO (loopStateResponses ls)
      let respVar = resps Map.! corrId
      atomically (putTMVar respVar replyMsg)
    Later {} -> do
      undefined

runActor :: LoopState -> RemoteRef -> Free ActorF a -> IO a
runActor ls self = iterM go return
  where
    go :: ActorF (IO a) -> IO a
    go (Call lref msg k) = do
      Just actor <- lookupActor lref (loopStateActors ls)
      Now reply <- runActor ls (localToRemoteRef (loopStateName ls) lref) (actor msg)
      emit ls (LogInvoke self lref msg reply)
      k reply
    go (RemoteCall rref msg k) = do
      (corrId, respTMVar) <- atomically $ do
        corrId <- readTVar (loopStateNextCorrelationId ls)
        modifyTVar' (loopStateNextCorrelationId ls) succ
        respTMVar <- newEmptyTMVar
        modifyTVar' (loopStateResponses ls) (Map.insert corrId respTMVar)
        return (corrId, respTMVar)
      let envelope = Envelope RequestKind self msg rref corrId
      say ls ("RemoteCall: " ++ show envelope)
      transportSend (loopStateTransport ls) envelope
      a <- async $ atomically $ do
        resp <- takeTMVar respTMVar -- XXX: timeout?
        modifyTVar' (loopStateResponses ls) (Map.delete corrId)
        modifyTVar' (loopStateWaitingAsyncs ls) (Map.delete corrId)
        return resp
      -- Associate the correlation id with the `Async` `a`, so that we can later
      -- install continuations for it.
      say ls ("correlating remote call `" ++ show corrId ++ "'")
      correlateAsync corrId a ls
      emit ls (LogSendStart self rref msg corrId)
      k a
    go (AsyncIO m k) = do
      a <- async m -- XXX: Use `asyncOn` a different capability than the main
                   -- event loop is running on. Or queue up the async to some
                   -- queue which a worker pool, running on different
                   -- capabilities, process.
      atomically (modifyTVar' (loopStateIOAsyncs ls) (a :))
      k a
    go (On (Left a) c k) = do
      let c' = \msg -> c (Left msg)
      -- XXX: install continuation
      undefined
      k ()
    go (On (Right a) c k) = undefined
    go (Await (Left a) k) = do
      reply <- wait a
      k (Left reply)
    go (Await (Right a) k) = do
      x <- wait a
      k (Right x)
    go (Get k) = do
     states <- readTVarIO (loopStateActorState ls)
     let state = states Map.! remoteToLocalRef self
     k state
    go (Put state' k) = do
      atomically (modifyTVar' (loopStateActorState ls)
                   (Map.insert (remoteToLocalRef self) state'))
      k ()

quit :: EventLoopRef -> IO ()
quit r = atomically $
  writeTBQueue (loopStateQueue (loopRefLoopState r)) (Command Quit)

helper :: EventLoopRef -> (TMVar a -> Command) -> IO a
helper r cmd = do
  respVar <- atomically $ do
    respVar <- newEmptyTMVar
    writeTBQueue (loopStateQueue (loopRefLoopState r)) (Command (cmd respVar))
    return respVar
  atomically (takeTMVar respVar)

spawn :: EventLoopRef -> (Message -> Actor) -> IO LocalRef
spawn r actor = helper r (Spawn actor)

invoke :: EventLoopRef -> LocalRef -> Message -> IO Message
invoke r lref msg =
  runActor
    (loopRefLoopState r)
    (dummyDeveloperRef r)
    (Free (Call lref msg return))

send :: EventLoopRef -> RemoteRef -> Message -> IO (Async Message)
send r rref msg =
  runActor
    (loopRefLoopState r)
    (dummyDeveloperRef r)
    (Free (RemoteCall rref msg return))
