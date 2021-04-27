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

data EventLoopRef = EventLoopRef (Async ())

data LoopState = LoopState
  { loopStateAsync :: TMVar (Async ()) -- | Hold the `Async` of the event loop itself.
  , loopStateQueue :: TBQueue Event
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
  return (EventLoopRef a)

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
handleCommand Quit ls = do
  a <- atomically (takeTMVar (loopStateAsync ls))
  cancel a
