module StuntDouble.EventLoop.AsyncHandler where

import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Monad
import Data.List

------------------------------------------------------------------------

-- XXX: reuse
data Event = Response Response

type Message = String

data Response = Receive (Async Message) Message

data LoopState = LoopState
  { loopStateQueue  :: TBQueue Event
  , loopStateAsyncs :: TVar [Async Message]
  }

------------------------------------------------------------------------

handleAsyncs :: LoopState -> IO ()
handleAsyncs ls = forever go
  where
    go = atomically $ do
      -- XXX: Use waitAnyCatchSTM and handle exceptions appropriately here, e.g.
      -- by extending `Response` with `Fail` and `Info`.
      as <- readTVar (loopStateAsyncs ls)
      (a, msg) <- waitAnySTM as
      writeTBQueue (loopStateQueue ls) (Response (Receive a msg))
      writeTVar (loopStateAsyncs ls) (delete a as)
