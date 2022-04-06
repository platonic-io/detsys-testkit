{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Experiment.Simulation where

import Control.Concurrent.Async (async)
import Control.Concurrent.MVar (MVar, newMVar, newEmptyMVar, takeMVar, putMVar)
import Control.Monad (void)
import Data.IORef (IORef, newIORef, writeIORef, readIORef)
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Time as UTCTime

type Time = UTCTime.UTCTime

isBefore :: Time -> Time -> Bool
isBefore t0 t1 = t0 <= t1

data EventLoopEvent -- nice name
  = ELEMessage Time Message
  | ELEQuit
  deriving (Eq, Show)

type ClientId = String
type NodeId = String

data Message
  = ClientRequest ClientId String
  | InternalMessage NodeId String
  | Tick
  deriving (Eq, Show)

data OutMessage
  = OMClientResponse ClientId String
  | OMInternalMessage NodeId String
  deriving (Eq, Show)

type Response = [OutMessage]

-- Interfaces
data IStateMachine state = IStateMachine
  { receive :: Time -> state -> Message -> IO (state, Response)
  , tick :: Time -> state -> IO (state, Response)
  , nextInterestingTime :: state -> IO Time
  }

data IQueue = IQueue
  { select :: IO EventLoopEvent } -- could be blocking

data ITimer = ITimer
  { registerTime :: Time -> IO ()}

data INetwork = INetwork
  { sendResponse :: Response -> IO ()}

-- Event loop

eventLoop
  :: forall state
   . IQueue
  -> ITimer
  -> INetwork
  -> IStateMachine state
  -> state
  -> IO state
eventLoop queue timer network stateMachine = go
  where
    go :: state -> IO state
    go state = do
      event <- select queue
      case event of
        ELEMessage time message -> do
          (state', response) <- receive stateMachine time state message
          timeToRegister <- nextInterestingTime stateMachine state'
          registerTime timer timeToRegister
          sendResponse network response
          go state'
        ELEQuit -> return state

-- Replay

type Journal = [(Time, Message)]

nullTimer :: ITimer
nullTimer = ITimer (const (return ()))

nullNetwork :: INetwork
nullNetwork = INetwork (const (return ()))

makeQueue :: Journal -> IO IQueue
makeQueue journal = do
  ref <- newIORef journal
  return (IQueue (readFrom ref))
  where
    readFrom :: IORef Journal -> IO EventLoopEvent
    readFrom ref = do
      xs <- readIORef ref
      case xs of
        [] -> return ELEQuit
        (time, message):xs' -> do
          writeIORef ref xs'
          return (ELEMessage time message)

replay :: [(Time, Message)] -> IStateMachine state -> state -> IO state
replay journal stateMachine state = do
  queue <- makeQueue journal
  eventLoop queue nullTimer nullNetwork stateMachine state

-- Simulation

-- TODO implment this
type Heap k v = [(k,v)]

type Agenda = Heap Time (NodeId, EventLoopEvent)

data AgendaView
  = NoMore
  | Item Time NodeId EventLoopEvent Agenda

dequeue :: Agenda -> AgendaView
dequeue [] = NoMore
dequeue ((time, (nodeId, event)):xs) = Item time nodeId event xs

insert1Agenda :: Agenda -> OutMessage -> Agenda
insert1Agenda agenda om = undefined -- TODO implement

insertResponseToAgenda :: Response -> Agenda -> Agenda
insertResponseToAgenda rs agenda = foldl insert1Agenda agenda rs

data SimulationState = SimulationState
  { ssEndOfTime :: Time
  , ssAgenda :: Agenda
  , ssNodes :: [NodeId]
  , ssSync :: SimulationSync
  }


data SimulationNodeSync = SimulationNodeSync
  { snsQueue :: MVar EventLoopEvent
  , snsResponses :: MVar Response
  }

data SimulationSync = SimulationSync
  { ssNode :: Map NodeId SimulationNodeSync
  }

makeSimulationSync :: [NodeId] -> IO SimulationSync
makeSimulationSync nodes = do
  xs <- mapM mkNodeSync nodes
  return (SimulationSync (Map.fromList xs))
  where
    mkNodeSync nodeId = do
      q <- newEmptyMVar
      r <- newEmptyMVar
      return (nodeId, SimulationNodeSync q r)

makeSimulationState :: Agenda -> Time -> [NodeId] -> IO SimulationState
makeSimulationState originalAgenda endOfTime nodes = do
  sync <- makeSimulationSync nodes
  return (SimulationState endOfTime originalAgenda nodes sync)

simulationQueue :: SimulationSync -> NodeId -> IQueue
simulationQueue sync nodeId =
  let mvar = snsQueue (ssNode sync Map.! nodeId)
  in IQueue (takeMVar mvar)

-- TODO implement this
simulationTimer :: SimulationSync -> NodeId -> ITimer
simulationTimer _ _ = ITimer (const (return ()))

simulationNetwork :: SimulationSync -> NodeId -> INetwork
simulationNetwork sync nodeId =
  let mvar = snsResponses (ssNode sync Map.! nodeId)
  in INetwork (putMVar mvar)

coordinator :: SimulationState -> IO ()
coordinator sState = go (ssAgenda sState)
  where
    go agenda = case dequeue (ssAgenda sState) of
      NoMore -> quitAll
      Item time nodeId event agenda'
        | ssEndOfTime sState `isBefore` time -> quitAll
        | otherwise -> do
            let sns = ssNode (ssSync sState) Map.! nodeId
            putMVar (snsQueue sns) event
            responses <- takeMVar (snsResponses sns)
            go (insertResponseToAgenda responses agenda')
    quitAll = mapM_ (\nodeId -> putMVar (snsQueue (ssNode (ssSync sState) Map.! nodeId)) ELEQuit) (ssNodes sState)

-- what do we want to return? History?
simulate :: Agenda -> Map NodeId state -> Time -> IStateMachine state -> IO ()
simulate originalAgenda originalState endTime stateMachine = do
  simulationState <- makeSimulationState originalAgenda endTime (map fst (Map.toAscList originalState))
  let simulationSync = ssSync simulationState
  mapM_ (\(nodeId, state) -> async $ void $
          eventLoop
            (simulationQueue   simulationSync nodeId)
            (simulationTimer   simulationSync nodeId)
            (simulationNetwork simulationSync nodeId)
            stateMachine state)
       (Map.toAscList originalState)
  coordinator simulationState
