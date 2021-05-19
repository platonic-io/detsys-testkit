{-# LANGUAGE OverloadedStrings #-}

module StuntDouble.SchedulerTest where

import Data.Text (Text)
import Control.Concurrent.Async
import Test.HUnit

import StuntDouble

------------------------------------------------------------------------

executorPort :: Int
executorPort = 3004

fakeExecutor :: IO ()
fakeExecutor = do
  t <- httpTransport executorPort
  e <- transportReceive t
  envelopeMessage e @?= envelopeMessage e -- XXX: check if cmd is of the right shape
  let resp = replyEnvelope e (Message "XXX: needs the right shape")
  transportSend t resp

fakeScheduler :: RemoteRef -> Message -> Actor
fakeScheduler executor (Message "step") = do
  (cmd, heap') <- "heap" ^. pop
  "heap" .= (heap' :: Datatype)
  a <- remoteCall executor (Message (prettyCommand cmd))
  Left (Just resp) <- unsafeAwait (Left a)
  -- assert resp -- XXX: check if of the right shape
  now <- get "time"
  seed <- get "seed"
  arrivalTime <- genArrivalTime now seed
  op2 push arrivalTime (parseCommand resp) %= "heap"
  return (Now (Message "stepped"))

  where
    parseCommand :: Message -> Datatype
    parseCommand (Message m) = Pair (Text "command") (Map undefined {- XXX: args -})

    prettyCommand :: Text -> String
    prettyCommand _ = "XXX: command"

unit_scheduler :: Assertion
unit_scheduler = do
  aExecutor <- async fakeExecutor
  elog <- emptyEventLog
  let ev = EventLoopName "scheduler"
  el <- makeEventLoop "/tmp" ev elog

  let executorRef = RemoteRef ("http://localhost:" ++ show executorPort) 0
      initState = stateFromList [ ("heap", emptyHeap)
                                , ("time", Timestamp undefined)
                                , ("seed", Integer 0)
                                ]
  lref <- spawn el (fakeScheduler executorRef) initState
  a <- send el (localToRemoteRef ev lref) (Message "step")
  reply <- wait a
  reply @?= Just (Message "stepped")

  quit el
  cancel aExecutor
