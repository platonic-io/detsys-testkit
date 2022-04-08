{-# LANGUAGE ScopedTypeVariables #-}

module ATMC.Lec5SimulationTestingV3 where

import Control.Concurrent.Async
import Control.Concurrent.MVar
import Control.Concurrent.STM
import Control.Exception
import Data.ByteString.Lazy (ByteString)
import Data.IORef
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import Data.Time
import Data.Typeable

import ATMC.Lec5.Agenda
import ATMC.Lec5.AwaitingClients
import ATMC.Lec5.Codec
import ATMC.Lec5.Event
import ATMC.Lec5.Network
import ATMC.Lec5.Options
import ATMC.Lec5.StateMachine
import ATMC.Lec5.Time

------------------------------------------------------------------------

newtype Topology = Topology (IntMap SomeCodecSM)

lookupReceiver :: NodeId -> Topology -> Maybe SomeCodecSM
lookupReceiver (NodeId nid) (Topology im) = IntMap.lookup nid im

eventLoopProduction :: [SomeCodecSM] -> IO ()
eventLoopProduction
  = eventLoop (Options Production)
  . Topology
  . IntMap.fromList
  . zip [0..]

newClock :: Deployment -> IO Clock
newClock Production           = realClock
newClock (Simulation _agenda) = fakeClockEpoch

eventLoop :: Options -> Topology -> IO ()
eventLoop opts topo = do
  putStrLn ("Starting event loop in " ++ show (oDeployment opts) ++
            " mode on port: "  ++ show pORT)
  ac    <- newAwaitingClients
  clock <- newClock (oDeployment opts)
  net   <- newNetwork (oDeployment opts) ac clock
  withAsync (nRun net) $ \anet -> do
    link anet
    runWorker topo ac clock net [anet]

runWorker :: Topology -> AwaitingClients -> Clock -> Network -> [Async ()]
          -> IO ()
runWorker topo ac clock net pids = go
  where
    go :: IO ()
    go = do
      event <- atomically (NetworkEvent <$> nRecv net) -- <|> TimerEvent <$>...
      if isExitCommand event
      then exit
      else do
        cSetCurrentTime clock (eventTime event) -- This is a noop in production deployment.
        handleEvent event
        go

    exit :: IO ()
    exit = mapM_ cancel pids

    handleEvent :: Event -> IO ()
    handleEvent (NetworkEvent (RawInput nodeId rawInput)) =
      case lookupReceiver nodeId topo of
        Nothing -> putStrLn ("Lookup of receiver failed, node id: " ++ show (unNodeId nodeId))
        Just (SomeCodecSM codec (SM state step)) ->
          case decodeInput codec rawInput of
            Nothing -> putStrLn (("Decoding of input failed, node id: " ++
                                  show (unNodeId nodeId)) ++ ", input: " ++
                                  show rawInput)
            Just input -> do
              r <- try (evaluate (step input state))
              case r of
                Left (e :: SomeException) ->
                  putStrLn ("step failed, error: " ++ displayException e)
                Right (outputs, state') ->
                  mapM_ (handleOutput codec nodeId) outputs

    handleEvent (TimerEvent) = undefined
    handleEvent (CommandEvent Exit) = error "IMPOSSIBLE: this case has already been handled"

    handleOutput :: Codec req msg resp -> NodeId -> Output resp msg -> IO ()
    handleOutput codec _fromNodeId (ClientResponse clientId response) =
      respondToAwaitingClient ac clientId (cEncodeResponse codec response)
    handleOutput codec fromNodeId (InternalMessageOut toNodeId msg) =
      nSend net fromNodeId toNodeId (cEncodeMessage codec msg)
