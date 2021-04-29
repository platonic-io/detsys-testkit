module StuntDouble.EventLoop.State where

import Data.Map (Map)
import qualified Data.Map as Map
import Control.Concurrent.STM
import Control.Concurrent.Async

import StuntDouble.EventLoop.Event
import StuntDouble.EventLoop.Transport
import StuntDouble.Actor
import StuntDouble.Reference
import StuntDouble.Message

------------------------------------------------------------------------


data LoopState = LoopState
  { loopStatePids :: TVar [Async ()] -- | Holds the `Async`s (or PIDs) of the
                                     --   event loop itself.
  , loopStateQueue :: TBQueue Event
  , loopStateActors :: TVar (Map LocalRef (Message -> Actor)) -- XXX: Only changed by main loop, so no need for STM?
  -- , loopStateHandlers :: TVar (Map (Async Message) (Message -> Actor))
  , loopStateIOHandlers :: TVar (Map (Async IOResult) (LocalRef, IOResult -> Actor))
  , loopStateIOAsyncs :: TVar [Async IOResult]
  , loopStateTransport :: Transport IO -- Will not change once created, so doesn't need STM?
  }


lookupActor :: LocalRef -> TVar (Map LocalRef (Message -> Actor))
            -> IO (Maybe (Message -> Actor))
lookupActor key var = Map.lookup key <$> atomically (readTVar var)
