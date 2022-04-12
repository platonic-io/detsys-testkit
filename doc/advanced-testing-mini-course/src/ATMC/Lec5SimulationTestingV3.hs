{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

module ATMC.Lec5SimulationTestingV3 where

import Control.Applicative
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
import ATMC.Lec5.Codec
import ATMC.Lec5.History
import ATMC.Lec5.Event
import ATMC.Lec5.Network
import ATMC.Lec5.Options
import ATMC.Lec5.StateMachine
import ATMC.Lec5.Time

------------------------------------------------------------------------

newtype Configuration = Configuration (IntMap SomeCodecSM)

lookupReceiver :: NodeId -> Configuration -> Maybe SomeCodecSM
lookupReceiver (NodeId nid) (Configuration im) = IntMap.lookup nid im

newClock :: Deployment -> IO Clock
newClock Production           = realClock
newClock (Simulation _agenda) = fakeClockEpoch

eventLoopProduction :: [SomeCodecSM] -> IO ()
eventLoopProduction
  = eventLoop (Options Production)
  . Configuration
  . IntMap.fromList
  . zip [0..]

eventLoopSimulation :: Agenda -> [SomeCodecSM] -> IO ()
eventLoopSimulation agenda
  = eventLoop (Options (Simulation agenda))
  . Configuration
  . IntMap.fromList
  . zip [0..]

echoAgenda :: Agenda
echoAgenda = makeAgenda
  [(epoch, RawInput (NodeId 0) (ClientRequest epoch (ClientId 0) "hi"))]

eventLoop :: Options -> Configuration -> IO ()
eventLoop opts config = do
  putStrLn ("Starting event loop in " ++ show (oDeployment opts) ++
            " mode on port: "  ++ show pORT)
  clock <- newClock (oDeployment opts)
  cmdQ  <- newTBQueueIO 1
  net   <- newNetwork (oDeployment opts) clock cmdQ
  withAsync (nRun net) $ \anet -> do
    link anet
    runWorker config clock net cmdQ [anet]

runWorker :: Configuration -> Clock -> Network -> TBQueue CommandEvent -> [Async ()] -> IO ()
runWorker config clock net cmdQ pids = go
  where
    go :: IO ()
    go = do
      event <- atomically
                  $  (NetworkEvent <$> nRecv net)
                 <|> (CommandEvent <$> readTBQueue cmdQ)
              -- <|> (TimerEvent <$> ...)
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
      case lookupReceiver nodeId config of
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
                Right (outputs, state') -> do
                  -- XXX: Save this somewhere...
                  let _e = HistEvent nodeId state input state' outputs
                  mapM_ (handleOutput codec nodeId) outputs
      where
        handleOutput :: Codec req msg resp -> NodeId -> Output resp msg -> IO ()
        handleOutput codec _fromNodeId (ClientResponse clientId response) =
          nRespond net clientId (cEncodeResponse codec response)
        handleOutput codec fromNodeId (InternalMessageOut toNodeId msg) =
          nSend net fromNodeId toNodeId (cEncodeMessage codec msg)
        handleOutput _codec fromNodeId (RegisterTimerSeconds secs) =
          undefined -- registerTimer timerWheel clock fromNodeId secs
        handleOutput _codec fromNodeId (ResetTimerSeconds secs) =
          undefined -- resetTimer timerWheel clock fromNodeId secs

    handleEvent (TimerEvent) = undefined
    handleEvent (CommandEvent Exit) = error "IMPOSSIBLE: this case has already been handled"
