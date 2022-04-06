module ATMC.Lec5SimulationTestingV3 where

import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Concurrent.MVar
import Data.IORef
import Data.Typeable
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import Data.ByteString.Lazy (ByteString)
import Data.Time

import ATMC.Lec5.Time
import ATMC.Lec5.EventQueue
import ATMC.Lec5.AwaitingClients
import ATMC.Lec5.StateMachine
import ATMC.Lec5.Options
import ATMC.Lec5.Network
import ATMC.Lec5.Codec

------------------------------------------------------------------------

newtype Topology = Topology (IntMap SomeCodecSM)

lookupReceiver :: NodeId -> Topology -> Maybe SomeCodecSM
lookupReceiver (NodeId nid) (Topology im) = IntMap.lookup nid im

eventLoop :: Options -> Topology -> IO ()
eventLoop opts topo = do
  queue <- newEventQueue
  ac    <- newAwaitingClients
  clock <- newClock (oDeployment opts)
  net   <- newNetwork (oDeployment opts) queue ac clock
  withAsync (nRun net) $ \anet -> do
    link anet
    runWorker topo queue ac clock net [anet]

runWorker :: Topology -> EventQueue -> AwaitingClients -> Clock -> Network -> [Async ()]
          -> IO ()
runWorker topo queue ac clock net pids = go
  where
    go :: IO ()
    go = do
      event <- dequeueEvent queue
      if isExitCommand event
      then exit
      else do
        cSetCurrentTime clock (eventTime event) -- This is a noop in production deployment.
        handleEvent event
        go

    exit :: IO ()
    exit = mapM_ cancel pids

    handleEvent :: Event -> IO ()
    handleEvent (NetworkEvent (RawInput (ClientRequest at clientId nodeId req))) =
      case lookupReceiver nodeId topo of
        Nothing -> return () -- XXX: Log?
        Just (SomeCodecSM codec (SM state step)) -> case cDecode codec req of
          Nothing -> return ()
          Just input -> do
            let (outputs, state') = step input state
            mapM_ (handleOutput codec) outputs

    handleEvent (TimerEvent) = undefined
    handleEvent (CommandEvent Exit) = error "__IMPOSSIBLE__"

    handleOutput codec (ClientResponse clientId response) =
      respondToAwaitingClient ac clientId (cEncode codec (Left response))
    handleOutput codec (InternalMessageOut nodeId msg) =
      nSend net nodeId (cEncode codec (Right msg))
