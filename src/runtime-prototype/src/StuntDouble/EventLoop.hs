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

initLoopState :: EventLoopName -> Transport IO -> IO LoopState
initLoopState name transport =
  LoopState
    <$> pure name
    <*> newTVarIO []
    <*> newTBQueueIO 128
    <*> newTVarIO Map.empty
    <*> newTVarIO Map.empty
    <*> newTVarIO []
    <*> pure transport
    <*> newTVarIO 0
    <*> newTVarIO Map.empty

makeEventLoop :: FilePath -> EventLoopName -> IO EventLoopRef
makeEventLoop fp name = do
  transport <- namedPipeTransport fp name
  ls <- initLoopState name transport
  aInHandler <- async (handleInbound ls)
  aEvHandler <- async (handleEvents ls)
  atomically (modifyTVar' (loopStatePids ls) ([aInHandler, aEvHandler] ++ ))
  return (EventLoopRef ls)

handleEvents :: LoopState -> IO ()
handleEvents ls = go
  where
    go = do
      e <- atomically (readTBQueue (loopStateQueue ls))
      putStr (getEventLoopName (loopStateName ls) ++ "> ")
      putStrLn (eventName e)
      handleEvent e ls
        `catch` \(exception :: SomeException) -> print exception
      go

handleEvent :: Event -> LoopState -> IO ()
handleEvent (Command c)  ls = handleCommand c ls
handleEvent (Response r) ls = handleResponse r ls
handleEvent (Receive r)  ls = handleReceive r ls

dummyDeveloperRef :: LoopState -> RemoteRef
dummyDeveloperRef ls = RemoteRef (getEventLoopName (loopStateName ls)) 0

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
  (corrId, respTMVar) <- atomically $ do
    corrId <- readTVar (loopStateNextCorrelationId ls)
    modifyTVar' (loopStateNextCorrelationId ls) succ
    respTMVar <- newEmptyTMVar
    modifyTVar' (loopStateResponses ls) (Map.insert corrId respTMVar)
    return (corrId, respTMVar)
  transportSend (loopStateTransport ls) (Envelope (dummyDeveloperRef ls) msg rref corrId)
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
  | otherwise = undefined

data ActorNotFound = ActorNotFound RemoteRef
  deriving Show

instance Exception ActorNotFound where

handleReceive :: Receive -> LoopState -> IO ()
handleReceive (Request e) ls = do
  mActor <- lookupActor (remoteToLocalRef (envelopeReceiver e)) (loopStateActors ls)
  case mActor of
    -- XXX: Throw here or just log and continue? Or take care of this at one
    -- layer above transport where we authenticate and verify messages are only
    -- going to known actors, i.e. that the remote refs are valid.
    Nothing -> throwIO (ActorNotFound (envelopeReceiver e))
    Just actor -> do
      Now replyMsg <- runActor ls (actor (envelopeMessage e))
      transportSend (loopStateTransport ls) (reply e replyMsg)

runActor :: LoopState -> Free ActorF a -> IO a
runActor ls = iterM go return
  where
    go :: ActorF (IO a) -> IO a
    go (Call lref msg k) = do
      Just actor <- lookupActor lref (loopStateActors ls)
      Now reply <- runActor ls (actor msg)
      k reply
    go (RemoteCall rref msg k) = do
      undefined
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

test :: IO ()
test = do
  el1 <- makeEventLoop "/tmp" "a"
  el2 <- makeEventLoop "/tmp" "b"
  lref <- spawn el1 testActor
  let msg = Message "hi"
  reply <- invoke el1 lref msg
  print reply
  a <- send el2 (localToRemoteRef "a" lref) msg
  reply' <- wait a
  print reply'
  threadDelay 10000
  quit el1
  quit el2
