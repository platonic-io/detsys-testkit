module StuntDouble.EventLoop.AsyncIOHandler where

import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Monad
import Data.List

import StuntDouble.EventLoop.State
import StuntDouble.EventLoop.Event

------------------------------------------------------------------------

handleAsyncIO :: LoopState -> IO ()
handleAsyncIO ls = forever go
  where
    go = atomically $ do
      -- XXX: Use waitAnyCatchSTM and handle exceptions appropriately here, e.g.
      -- by extending `AsyncIODone` with `Fail` and `Info`.
      as <- readTVar (loopStateIOAsyncs ls)
      (a, ioResult) <- waitAnySTM as
      writeTBQueue (loopStateQueue ls) (AsyncIODone a ioResult)
      writeTVar (loopStateIOAsyncs ls) (delete a as)
