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
import StuntDouble.Message
import StuntDouble.Reference
import StuntDouble.EventLoop.Transport
import StuntDouble.FreeMonad
import StuntDouble.EventLoop.State
import StuntDouble.EventLoop.Event
import StuntDouble.EventLoop.InboundHandler

------------------------------------------------------------------------

newtype EventLoopRef = EventLoopRef
  { loopRefLoopState :: LoopState }

eventLog :: EventLoopRef -> IO EventLog
eventLog = fmap reverse . readTVarIO . loopStateEventLog . loopRefLoopState

dummyDeveloperRef :: EventLoopRef -> RemoteRef
dummyDeveloperRef r = RemoteRef (getEventLoopName (loopStateName ls)) dummyIndex
  where
    ls = loopRefLoopState r
    dummyIndex = -1

------------------------------------------------------------------------

initLoopState :: EventLoopName -> Transport IO -> TVar [String] -> IO LoopState
initLoopState name transport logs =
  LoopState
    <$> pure name
    <*> newTVarIO []
    <*> newTBQueueIO 128
    <*> newTVarIO Map.empty
    <*> newTVarIO Map.empty
    <*> newTVarIO []
    <*> pure transport
    <*> newTVarIO (if name == eventLoopA then 0 else 100) -- XXX: fix by
                                                          -- namespacing
                                                          -- corr ids by
                                                          -- loop name?
    <*> newTVarIO Map.empty
    <*> newTVarIO Map.empty
    <*> newTVarIO Map.empty
    <*> pure logs
    <*> newTVarIO []

makeEventLoop :: FilePath -> EventLoopName -> IO EventLoopRef
makeEventLoop fp name = do
  transport <- namedPipeTransport fp name
  logs <- newTVarIO []
  ls <- initLoopState name transport logs
  aInHandler <- async (handleInbound ls)
  aEvHandler <- async (handleEvents ls)
  atomically (modifyTVar' (loopStatePids ls) ([aInHandler, aEvHandler] ++ ))
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
-- handleEvent (Response r) ls = handleResponse r ls
handleEvent (Receive r)  ls = handleReceive r ls

handleCommand :: Command -> LoopState -> IO ()
handleCommand (Spawn actor respVar) ls = atomically $ do
  actors <- readTVar (loopStateActors ls)
  let lref = LocalRef (Map.size actors)
  writeTVar (loopStateActors ls) (Map.insert lref actor actors)
  putTMVar respVar lref
handleCommand Quit ls = do
  pids <- atomically (readTVar (loopStatePids ls))
  threadDelay 100000
  mapM_ cancel pids

  {-
handleResponse :: Response -> LoopState -> IO ()
handleResponse (Reply respTMVar e) ls = do
  say ls ("handleResponse: Reply: " ++ show e)
  atomically (putTMVar respTMVar (envelopeMessage e))
handleResponse (AsyncReply respTMVar a e) ls = do
  say ls ("handleResponse: AsyncReply: " ++ show e)
  say ls "recalling continuation"
  (self, k) <- recallContinuation a ls
  cont <- runActor ls self (k (envelopeMessage e))
  case cont of
    Now replyMsg -> do
      say ls ("Now: " ++ show replyMsg)
      -- Just corrId <- reverseCorrelateAsync a ls
      -- say ls ("Now: CorrId: " ++ show corrId)
      -- resps <- readTVarIO (loopStateResponses ls)
      -- let respVar = resps Map.! corrId
      -- say ls ("Now: loopStateResponses.keys = " ++ show (Map.keys resps))
      atomically (putTMVar respTMVar replyMsg)
      say ls "Done!"
    Later {} -> do
      say ls "Later"
      undefined
-}

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
            transportSend (loopStateTransport ls) (reply e replyMsg)
          Later async k -> do
            -- The actor has to talk to other remote actors before being able to reply.
            installContinuation async (envelopeReceiver e) k ls

  ResponseKind -> do
    resps <- readTVarIO (loopStateResponses ls)
    let respVar = resps Map.! envelopeCorrelationId e
    asyncs <- readTVarIO (loopStateWaitingAsyncs ls)
    let async = asyncs Map.! envelopeCorrelationId e
    -- XXX: check if continuation exist
    -- (self, k) <- recallContinuation async ls
    -- cont <- runActor ls self (k (envelopeMessage e))
    emit ls (LogSendFinish (envelopeCorrelationId e) (envelopeMessage e))
    atomically (putTMVar respVar (envelopeMessage e))
    -- case cont of
    --   Now replyMsg -> do
    --     emit ls (LogSendFinish (envelopeCorrelationId e) replyMsg)
    --     atomically (putTMVar respVar replyMsg)
    --   Later {} -> do
    --     undefined

runActor :: LoopState -> RemoteRef -> Free ActorF a -> IO a
runActor ls self = iterM go return
  where
    go :: ActorF (IO a) -> IO a
    go (Call lref msg k) = do
      Just actor <- lookupActor lref (loopStateActors ls)
      Now reply <- runActor ls self (actor msg)
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
        return resp
      -- Associate the correlation id with the `Async` `a`, so that we can later
      -- install continuations for it.
      say ls ("correlating remote call `" ++ show corrId ++ "'")
      correlateAsync corrId a ls
      emit ls (LogSendStart self rref msg corrId)
      k a
    go (AsyncIO m k) = do
      a <- async m
      atomically (modifyTVar' (loopStateIOAsyncs ls) (a :))
      k a
    go (Get k) =  do
      undefined
    go (Put state' k) = do
      undefined

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

testActor :: Message -> Actor
testActor (Message "hi") = return (Now (Message "bye!"))

testActor2 :: RemoteRef -> Message -> Actor
testActor2 rref (Message "init") = do
  a <- remoteCall rref (Message "hi")
  return (Later a (\(Message msg) -> return (Now (Message ("Got: " ++ msg)))))

eventLoopA :: EventLoopName
eventLoopA = EventLoopName "event-loop-a"

eventLoopB :: EventLoopName
eventLoopB = EventLoopName "event-loop-b"

test1 :: IO ()
test1 = do
  el1 <- makeEventLoop "/tmp" eventLoopA
  el2 <- makeEventLoop "/tmp" eventLoopB
  lref <- spawn el1 testActor
  let msg = Message "hi"
  reply' <- invoke el1 lref msg
  -- say' logs (show reply')
  a <- send el1 (localToRemoteRef eventLoopA lref) msg
  reply'' <- wait a
  -- say' logs (show reply'')
  threadDelay 10000
  quit el1
  quit el2
  displayLogs (loopRefLoopState el1)

test2 :: EventLoopRef -> EventLoopRef -> IO ()
test2 el1 el2 = do
  lref1 <- spawn el1 testActor
  lref2 <- spawn el2 (testActor2 (localToRemoteRef eventLoopA lref1))
  a <- send el2 (localToRemoteRef eventLoopB lref2) (Message "init")
  -- say' logs "send done, waiting..."
  done <- wait a
  -- say' logs (show done)
  threadDelay 10000
  quit el1
  quit el2
  dumpState (loopRefLoopState el1)
  dumpState (loopRefLoopState el2)
  displayLogs (loopRefLoopState el1)

t1 :: IO ()
t1 = test1 `catch` (\(e :: SomeException) -> displayLogs undefined)

t2 :: IO ()
t2 = do
  el1 <- makeEventLoop "/tmp" eventLoopA
  el2 <- makeEventLoop "/tmp" eventLoopB
  test2 el1 el2
    `catch` (\(e :: SomeException) -> do
                dumpState (loopRefLoopState el1)
                dumpState (loopRefLoopState el2)
                displayLogs (loopRefLoopState el2))
