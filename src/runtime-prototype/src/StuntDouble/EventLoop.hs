module StuntDouble.EventLoop where

import Control.Monad
import Control.Concurrent
import Control.Concurrent.STM.TBQueue

import StuntDouble.Actor
import StuntDouble.Message
import StuntDouble.Reference

------------------------------------------------------------------------

data EventLoopId = EventLoopId

data Event
  = Spawn Actor
  | Call LocalRef Message
  | Send RemoteRef Message
  | Receive Message

data LoopState = LoopState
  { loopStateQueue :: TBQueue Event }

initLoopState :: IO LoopState
initLoopState = do
  queue <- newTBQueueIO 128
  return (LoopState queue)

makeEventLoop :: IO EventLoopId
makeEventLoop = do
  loopState <- initLoopState
  tid <- forkIO $ forever $ undefined loopState
  tid' <- forkIO $ forever $ undefined loopState
  undefined

handleAdminCommands :: TBQueue Event -> IO ()
handleAdminCommands queue = undefined
