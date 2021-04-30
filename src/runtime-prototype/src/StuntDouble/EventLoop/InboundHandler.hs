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
import StuntDouble.Message

------------------------------------------------------------------------

handleInbound :: LoopState -> IO ()
handleInbound ls = forever go
  where
    go = do
      e <- transportReceive (loopStateTransport ls)
      atomically $ do
        responses <- readTVar (loopStateResponses ls)
        let corrId = envelopeCorrelationId e
        case Map.lookup corrId responses of
          Nothing ->
            writeTBQueue (loopStateQueue ls) (Receive (Request e))
          Just respTMVar -> do
            -- XXX:
            -- if envelopeSender e == dummyDeveloperRef ls
            -- then writeTBQueue (loopStateQueue ls) (Receive (Request e))
            -- else do
              waitingAsyncs <- readTVar (loopStateWaitingAsyncs ls)
              case Map.lookup corrId waitingAsyncs of
                Nothing -> writeTBQueue (loopStateQueue ls) (Response (Reply respTMVar e))
                Just a  -> writeTBQueue (loopStateQueue ls) (Response (AsyncReply respTMVar a e))
