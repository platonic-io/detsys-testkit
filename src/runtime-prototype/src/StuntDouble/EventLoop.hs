module StuntDouble.EventLoop where

import Control.Exception
import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Concurrent.STM.TBQueue

import StuntDouble.Actor
import StuntDouble.Message
import StuntDouble.Reference

------------------------------------------------------------------------

data Command
  = Spawn (Message -> Actor)
  | Invoke LocalRef Message
  | Send RemoteRef Message
  | Quit

data Response
  = Receive AsyncRef Message

-- XXX:
type RequestId = Int
type AsyncRef = (Async Message, RequestId)

data Event = Command Command | Response Response

newtype EventLoopRef = EventLoopRef
  { loopRefLoopState :: LoopState }

data LoopState = LoopState
  { loopStateAsync :: TMVar (Async ()) -- | Hold the `Async` of the event loop itself.
  , loopStateQueue :: TBQueue Event
  , loopStateActors :: TVar (Map InternalActorRef (Message -> Actor))
  , loopStateHandlers :: TVar (Map RequestId (Message -> Actor))
  -- , loopStateBlockedResponses :: TVar
  -- we also need to have the state for each actor..
  }

------------------------------------------------------------------------

initLoopState :: IO LoopState
initLoopState = do
  a <- newEmptyTMVarIO
  queue <- newTBQueueIO 128
  return (LoopState a queue)

makeEventLoop :: IO EventLoopRef
makeEventLoop = do
  loopState <- initLoopState
  -- tid <- forkIO $ forever $ undefined loopState
  -- tid' <- forkIO $ forever $ undefined loopState
  a <- async (handleEvents loopState)
  atomically (putTMVar (loopStateAsync loopState) a)
  return (EventLoopRef loopState)

handleEvents :: LoopState -> IO ()
handleEvents ls = go
  where
    go = do
      e <- atomically (readTBQueue (loopStateQueue ls))
      handleEvent e ls
      go

handleEvent :: Event -> LoopState -> IO ()
handleEvent (Command c) ls = handleCommand c ls

handleCommand :: Command -> LoopState -> IO ()
handleCommand (Spawn actor) ls = do
  undefined
handleCommand (Invoke lr m) ls = do
  undefined
handleCommand (Send rr m) ls = do
  undefined
handleCommand Quit ls = do
  a <- atomically (takeTMVar (loopStateAsync ls))
  cancel a

runActor :: LoopState -> Actor -> IO ()
runActor ls = iterM go
  where
    go :: ActorF (IO a) -> IO a
    go (Call lref msg k) = do
      undefined
    go (RemoteCall rref msg k) = do
      undefined
    go (AsyncIO m k) = do
      x <- async m -- this should probably register this somewhere?
      k x
    go (Get k) =  do
      undefined
    go (Put state' k) = do
      undefined

quit :: EventLoopRef -> IO ()
quit r = atomically $
  writeTBQueue (loopStateQueue (loopRefLoopState r)) (Command Quit)

test = do
  r <- makeEventLoop
  quit r
