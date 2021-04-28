module StuntDouble.EventLoop.State where

import Data.Map (Map)
import Control.Concurrent.STM
import Control.Concurrent.Async

import StuntDouble.EventLoop.Event
import StuntDouble.EventLoop.Transport
import StuntDouble.Actor
import StuntDouble.Reference
import StuntDouble.Message

------------------------------------------------------------------------


data LoopState = LoopState
  { loopStateAsync :: TMVar (Async ()) -- | Hold the `Async` of the event loop itself.
  , loopStateQueue :: TBQueue Event
  , loopStateActors :: TVar (Map InternalActorRef (Message -> Actor))
  -- , loopStateHandlers :: TVar (Map (Async Message) (Message -> Actor))
  , loopStateIOHandlers :: TVar (Map (Async IOResult) (InternalActorRef, IOResult -> Actor))
  , loopStateIOAsyncs :: TVar [Async IOResult]
  , loopStateTransport :: Transport IO -- Will not change once created, so doesn't need STM?
  }
