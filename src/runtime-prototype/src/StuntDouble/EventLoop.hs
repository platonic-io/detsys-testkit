{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module StuntDouble.EventLoop where

import Data.Map (Map)
import qualified Data.Map as Map
import Control.Exception
import Control.Concurrent
import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Concurrent.STM.TBQueue

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
    <*> newTVarIO (if name == EventLoopName "a" then 0 else 100) -- XXX: fix by
                                                                 -- namespacing
                                                                 -- corr ids by
                                                                 -- loop name?
    <*> newTVarIO Map.empty
    <*> newTVarIO Map.empty
    <*> newTVarIO Map.empty
    <*> pure logs

makeEventLoop :: FilePath -> EventLoopName -> TVar [String] -> IO EventLoopRef
makeEventLoop fp name logs = do
  transport <- namedPipeTransport fp name
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
handleEvent (Response r) ls = handleResponse r ls
handleEvent (Receive r)  ls = handleReceive r ls

handleCommand :: Command -> LoopState -> IO ()
handleCommand (Spawn actor respVar) ls = atomically $ do
  actors <- readTVar (loopStateActors ls)
  let lref = LocalRef (Map.size actors)
  writeTVar (loopStateActors ls) (Map.insert lref actor actors)
  putTMVar respVar lref
handleCommand (Invoke lref msg respVar) ls = do
  Just actor <- lookupActor lref (loopStateActors ls)
  Now reply <- runActor ls (actor msg)
  atomically (putTMVar respVar reply)
handleCommand (Send rref msg respVar) ls = do
  say ls ("Send: " ++ show rref ++ " " ++ show msg)
  (corrId, respTMVar) <- atomically $ do
    corrId <- readTVar (loopStateNextCorrelationId ls)
    modifyTVar' (loopStateNextCorrelationId ls) succ
    respTMVar <- newEmptyTMVar
    modifyTVar' (loopStateResponses ls) (Map.insert corrId respTMVar)
    return (corrId, respTMVar)
  let e = Envelope (dummyDeveloperRef ls) msg rref corrId
  say ls ("Send: " ++ show e)
  -- transportSend (loopStateTransport ls) e
  atomically (writeTBQueue (loopStateQueue ls) (Receive (Request e)))
  a <- async $ atomically $ do
    resp <- takeTMVar respTMVar -- XXX: timeout?
    modifyTVar' (loopStateResponses ls) (Map.delete corrId)
    return resp
  atomically (putTMVar respVar a)
handleCommand Quit ls = do
  pids <- atomically (readTVar (loopStatePids ls))
  threadDelay 100000
  mapM_ cancel pids

handleResponse :: Response -> LoopState -> IO ()
handleResponse (Reply respTMVar e) ls
  | envelopeReceiver e == dummyDeveloperRef ls =
      atomically (putTMVar respTMVar (envelopeMessage e))
  | otherwise = do
      say ls (show e)
      atomically (putTMVar respTMVar (envelopeMessage e))
handleResponse (AsyncReply respTMVar a e) ls = do
  say ls "recalling continuation"
  k <- recallContinuation a ls
  cont <- runActor ls (k (envelopeMessage e))
  case cont of
    Now replyMsg -> do
      say ls ("Now: " ++ show replyMsg)
      atomically (putTMVar respTMVar replyMsg)
    Later {} -> do
      say ls "Later"
      undefined

data ActorNotFound = ActorNotFound RemoteRef
  deriving Show

instance Exception ActorNotFound where

handleReceive :: Receive -> LoopState -> IO ()
handleReceive (Request e) ls = do
  say ls ("handleReceive: Request: " ++ show e)
  mActor <- lookupActor (remoteToLocalRef (envelopeReceiver e)) (loopStateActors ls)
  case mActor of
    -- XXX: Throw here or just log and continue? Or take care of this at one
    -- layer above transport where we authenticate and verify messages are only
    -- going to known actors, i.e. that the remote refs are valid.
    Nothing -> throwIO (ActorNotFound (envelopeReceiver e))
    Just actor -> do
      cont <- runActor ls (actor (envelopeMessage e))
      case cont of
        Now replyMsg -> transportSend (loopStateTransport ls) (reply e replyMsg)
        Later async k -> do
          -- The actor has to talk to other remote actors before being able to reply.
          say ls "installing continuation"
          installContinuation async k ls

runActor :: LoopState -> Free ActorF a -> IO a
runActor ls = iterM go return
  where
    go :: ActorF (IO a) -> IO a
    go (Call lref msg k) = do
      Just actor <- lookupActor lref (loopStateActors ls)
      Now reply <- runActor ls (actor msg)
      k reply
    go (RemoteCall rref msg k) = do
      (corrId, respTMVar) <- atomically $ do
        corrId <- readTVar (loopStateNextCorrelationId ls)
        modifyTVar' (loopStateNextCorrelationId ls) succ
        respTMVar <- newEmptyTMVar
        modifyTVar' (loopStateResponses ls) (Map.insert corrId respTMVar)
        return (corrId, respTMVar)
      -- XXX: we need to remote ref of the actor currently run here...
      let from = RemoteRef "b" 0
          envelope = Envelope from msg rref corrId
      say ls ("RemoteCall: " ++ show envelope)
      transportSend (loopStateTransport ls) envelope
      a <- async $ atomically $ do
        resp <- takeTMVar respTMVar -- XXX: timeout?
        modifyTVar' (loopStateResponses ls) (Map.delete corrId)
        return resp
      -- Associate the correlation id with the `Async` `a`, so that we can later
      -- install continuations for it.
      say ls ("correlating `" ++ show corrId ++ "'")
      correlateAsync corrId a ls
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
invoke r lref msg = helper r (Invoke lref msg)

send :: EventLoopRef -> RemoteRef -> Message -> IO (Async Message)
send r rref msg = helper r (Send rref msg)

testActor :: Message -> Actor
testActor (Message "hi") = return (Now (Message "bye!"))

testActor2 :: RemoteRef -> Message -> Actor
testActor2 rref (Message "init") = do
  a <- remoteCall rref (Message "hi")
  return (Later a (\(Message msg) -> return (Now (Message ("Got: " ++ msg)))))

test :: IO ()
test = do
  logs <- newTVarIO []
  el1 <- makeEventLoop "/tmp" "a" logs
  el2 <- makeEventLoop "/tmp" "b" logs
  lref <- spawn el1 testActor
  let msg = Message "hi"
  reply' <- invoke el1 lref msg
  say' logs (show reply')
  a <- send el2 (localToRemoteRef "a" lref) msg
  reply'' <- wait a
  say' logs (show reply'')
  threadDelay 10000
  quit el1
  quit el2
  displayLogs (loopRefLoopState el1)

test2 :: TVar [String] -> IO ()
test2 logs = do
  el1 <- makeEventLoop "/tmp" "a" logs
  el2 <- makeEventLoop "/tmp" "b" logs
  lref1 <- spawn el1 testActor
  lref2 <- spawn el2 (testActor2 (localToRemoteRef "a" lref1))
  a <- send el2 (localToRemoteRef "b" lref2) (Message "init")
  done <- wait a
  say' logs (show done)
  threadDelay 10000
  quit el1
  quit el2
  displayLogs (loopRefLoopState el1)

test3 :: IO ()
test3 = do
  logs <- newTVarIO []
  test2 logs
    `catch` (\(e :: SomeException) -> displayLogs' logs)
