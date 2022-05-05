{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

module Lec05.EventLoop where

import Control.Monad
import Control.Applicative
import Control.Concurrent.Async
import Control.Concurrent.MVar
import Control.Concurrent.STM
import Control.Exception
import Data.ByteString.Lazy (ByteString)
import Data.IORef
import Data.Time

import Lec05.Agenda
import Lec05.Codec
import Lec05.History
import Lec05.Event
import Lec05.EventQueue
import Lec05.Network
import Lec05.Options
import Lec05.Random
import Lec05.StateMachine
import Lec05.Time
import Lec05.TimerWheel
import Lec05.Configuration
import Lec05.Deployment

------------------------------------------------------------------------

eventLoopProduction :: [SomeCodecSM] -> IO ()
eventLoopProduction = eventLoop (Options Production) <=< makeConfiguration

eventLoopSimulation :: Seed -> Agenda -> History -> [SomeCodecSM] -> IO ()
eventLoopSimulation seed agenda history =
  eventLoop (Options (Simulation seed agenda history)) <=< makeConfiguration

echoAgenda :: Agenda
echoAgenda = makeAgenda
  [(epoch, NetworkEventE (NetworkEvent (NodeId 0) (ClientRequest epoch (ClientId 0) "hi")))]

eventLoop :: Options -> Configuration -> IO ()
eventLoop opts config = do
  putStrLn ("Starting event loop in " ++ displayDeploymentMode (oDeployment opts) ++
            " mode on port: "  ++ show pORT)
  d <- newDeployment (oDeployment opts) config
  withAsync (nRun (dNetwork d)) $ \anet -> do
    link anet
    withAsync (runTimerManager (dTimerWheel d) (dClock d) (dEventQueue d)) $ \atm -> do
      link atm
      runWorker (d { dPids = Pids [anet, atm] })

-- XXX: Faults

runWorker :: Deployment -> IO ()
runWorker d = go
  where
    go :: IO ()
    go = do
      event <- eqDequeue (dEventQueue d)
      if isExitCommand event
      then exit
      else do
        cSetCurrentTime (dClock d) (getEventTime event) -- This is a noop in production deployment.
        handleEvent event
        go

    exit :: IO ()
    exit = mapM_ cancel (unPids (dPids d))

    handleEvent :: Event -> IO ()
    handleEvent (NetworkEventE (NetworkEvent nodeId rawInput)) = do
      r <- lookupReceiver nodeId (dConfiguration d)
      case r of
        Nothing -> putStrLn ("Lookup of receiver failed, node id: " ++ show (unNodeId nodeId))
        Just (SomeCodecSM codec (SM state step _timeout)) ->
          case decodeInput codec rawInput of
            Nothing -> putStrLn (("Decoding of input failed, node id: " ++
                                  show (unNodeId nodeId)) ++ ", input: " ++
                                  show rawInput)
            Just input -> do
              gen <- rGetStdGen (dRandom d)
              r <- try (evaluate (step input state gen))
              case r of
                Left (e :: SomeException) ->
                  putStrLn ("step failed, error: " ++ displayException e)
                Right (outputs, state', gen') -> do
                  dAppendHistory d (HistEvent nodeId state input state' outputs)
                  updateReceiverState nodeId state' (dConfiguration d)
                  rSetStdGen (dRandom d) gen'
                  mapM_ (handleOutput codec nodeId) outputs

    handleEvent (TimerEventE (TimerEvent nodeId time)) = do
      r <- lookupReceiver nodeId (dConfiguration d)
      case r of
        Nothing -> putStrLn ("Lookup of receiver failed, node id: " ++ show (unNodeId nodeId))
        Just (SomeCodecSM codec (SM state _step timeout)) -> do
          gen <- rGetStdGen (dRandom d)
          r <- try (evaluate (timeout time state gen))
          case r of
            Left (e :: SomeException) ->
              putStrLn ("timeout failed, error: " ++ displayException e)
            Right (outputs, state', gen') -> do
              -- XXX: Append to history
              updateReceiverState nodeId state' (dConfiguration d)
              rSetStdGen (dRandom d) gen'
              mapM_ (handleOutput codec nodeId) outputs

    handleEvent (CommandEventE Exit) = error "IMPOSSIBLE: this case has already been handled"

    handleOutput :: Codec req msg resp -> NodeId -> Output resp msg -> IO ()
    handleOutput codec _fromNodeId (ClientResponse clientId response) =
      nRespond (dNetwork d) clientId (cEncodeResponse codec response)
    handleOutput codec fromNodeId (InternalMessageOut toNodeId msg) =
      nSend (dNetwork d) fromNodeId toNodeId (cEncodeMessage codec msg)
    handleOutput _codec fromNodeId (RegisterTimerSeconds secs) =
      registerTimer (dTimerWheel d) (dClock d) fromNodeId secs
    handleOutput _codec fromNodeId (ResetTimerSeconds secs) =
      resetTimer (dTimerWheel d) (dClock d) fromNodeId secs
