module StuntDouble.SchedulerTest where

import Control.Concurrent.Async
import Test.HUnit

import StuntDouble

------------------------------------------------------------------------

fakeExecutor :: IO ()
fakeExecutor = do
  let port = 3004
  t <- httpTransport port
  e <- transportReceive t
  envelopeMessage e @?= envelopeMessage e -- XXX: check if cmd is of the right shape
  let resp = replyEnvelope e (Message "XXX: needs the right shape")
  transportSend t resp

fakeScheduler :: RemoteRef -> Message -> Actor
fakeScheduler executor (Message "step") = do
  cmd <- undefined -- popHeap
  a <- remoteCall executor cmd
  resp <- unsafeAwait (Left a)
  -- assert resp -- XXX: check if of the right shape
  now <- undefined -- get "time" from state
  seed <- undefined -- get "seed"
  arrivalTime <- undefined -- genArrivalTime now seed
  -- pushHeap arrivalTime resp
  return (Now (Message "stepped"))

unit_scheduler :: Assertion
unit_scheduler = do
  aExecutor <- async fakeExecutor
  elog <- emptyEventLog
  let ev = EventLoopName "scheduler"
  el <- makeEventLoop "/tmp" ev elog

  let executorRef = RemoteRef "http://localhost:3004" 0
  lref <- spawn el (fakeScheduler executorRef)
  a <- send el (localToRemoteRef ev lref) (Message "step")
  reply <- wait a
  reply @?= Message "stepped"

  quit el
  cancel aExecutor
