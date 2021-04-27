module StuntDouble.EventLoop where

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
  { loopStateQueue :: TBQueue Event }

------------------------------------------------------------------------

initLoopState :: IO LoopState
initLoopState = do
  queue <- newTBQueueIO 128
  return (LoopState queue)

makeEventLoop :: IO EventLoopRef
makeEventLoop = do
  loopState <- initLoopState
  -- tid <- forkIO $ forever $ undefined loopState
  -- tid' <- forkIO $ forever $ undefined loopState
  a <- async (handleEvents loopState)
  return (EventLoopRef a)

handleEvents :: LoopState -> IO ()
handleEvents (LoopState queue) = go
  where
    go = do
      e <- atomically (readTBQueue queue)
      handleEvent e
      go

handleEvent :: Event -> IO ()
handleEvent = undefined
