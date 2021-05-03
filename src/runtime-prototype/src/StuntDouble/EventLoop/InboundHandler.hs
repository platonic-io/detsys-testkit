module StuntDouble.EventLoop.InboundHandler where

import qualified Data.Map as Map
import Control.Monad
import Control.Exception
import Control.Concurrent.Async
import Control.Concurrent.STM

import StuntDouble.EventLoop.State
import StuntDouble.EventLoop.Event
import StuntDouble.EventLoop.Transport
import StuntDouble.Reference

------------------------------------------------------------------------

handleInbound :: LoopState -> IO ()
handleInbound ls = forever go
  where
    go = do
      e <- transportReceive (loopStateTransport ls)
      atomically (writeTBQueue (loopStateQueue ls) (Receive e))
