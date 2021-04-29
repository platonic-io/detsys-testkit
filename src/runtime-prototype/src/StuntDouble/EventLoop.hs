{-# LANGUAGE ExistentialQuantification #-}
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
import StuntDouble.EventLoop.RequestHandler

------------------------------------------------------------------------

newtype EventLoopRef = EventLoopRef
  { loopRefLoopState :: LoopState }

------------------------------------------------------------------------

initLoopState :: Transport IO -> IO LoopState
initLoopState transport =
  LoopState
    <$> newTVarIO []
    <*> newTBQueueIO 128
    <*> newTVarIO Map.empty
    <*> newTVarIO Map.empty
    <*> newTVarIO []
    <*> pure transport

makeEventLoop :: FilePath -> IO EventLoopRef
makeEventLoop fp = do
  transport <- namedPipeTransport fp
  ls <- initLoopState transport
  aReqHandler <- async (handleRequests ls)
  aEvHandler <- async (handleEvents ls)
  atomically (modifyTVar' (loopStatePids ls) ([aReqHandler, aEvHandler] ++ ))
  return (EventLoopRef ls)

handleEvents :: LoopState -> IO ()
handleEvents ls = go
  where
    go = do
      e <- atomically (readTBQueue (loopStateQueue ls))
      putStrLn (eventName e)
      handleEvent e ls
        `catch` \(exception :: SomeException) -> print exception
      go

handleEvent :: Event -> LoopState -> IO ()
handleEvent (Command c) ls = handleCommand c ls
handleEvent (Receive r) ls = handleReceive r ls

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
handleCommand (Send rr m) ls = do
  -- a <- async (makeHttpRequest (translateToUrl rr) (seralise m))
  -- atomically (modifyTVar (loopStateAsyncs ls) (a :))
  undefined
handleCommand Quit ls = do
  pids <- atomically (readTVar (loopStatePids ls))
  threadDelay 100000
  mapM_ cancel pids

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
      _ <- runActor ls (actor (envelopeMessage e))
      return ()

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
      atomically (modifyTVar (loopStateIOAsyncs ls) (a :))
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

testActor :: Message -> Actor
testActor (Message "hi") = return (Now (Message "bye!"))

test :: IO ()
test = do
  r <- makeEventLoop "/tmp/a"
  lref <- spawn r testActor
  let msg = Message "hi"
  reply <- invoke r lref msg
  print reply
  quit r
