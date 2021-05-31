{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module StuntDouble.ActorMapTest where

import Control.Concurrent
import Control.Concurrent.Async
import Control.Exception
import Test.HUnit hiding (State)

import StuntDouble.Actor (IOResult(IOUnit, String))
import StuntDouble.Actor.State
import StuntDouble.ActorMap
import StuntDouble.Datatype
import StuntDouble.EventLoop.Transport
import StuntDouble.FreeMonad
import StuntDouble.Message
import StuntDouble.Random
import StuntDouble.Reference
import StuntDouble.Time

------------------------------------------------------------------------

withEventLoop :: EventLoopName -> (EventLoop -> FakeTimeHandle -> IO ()) -> IO ()
withEventLoop name k = do
  (time, h) <- fakeTimeEpoch
  el <- makeEventLoop time (makeSeed 0) (NamedPipe "/tmp") name
  k el h
  quit el

eventLoopA :: String -> EventLoopName
eventLoopA suffix = EventLoopName ("event-loop-actormap-a" ++ "-" ++ suffix)

testActor :: Message -> Actor
testActor (Message "hi") = Actor (return (Message "bye!"))

------------------------------------------------------------------------

unit_actorMapInvoke :: Assertion
unit_actorMapInvoke = withEventLoop (eventLoopA "invoke") $ \el _h -> do
  lref <- spawn el testActor emptyState
  reply <- ainvoke el lref (Message "hi")
  reply @?= Message "bye!"

unit_actorMapSend :: Assertion
unit_actorMapSend = do
  let ev = eventLoopA "send"
  withEventLoop ev $ \el _h -> do
    lref <- spawn el testActor emptyState
    let rref = localToRemoteRef ev lref
    a <- asend el rref (Message "hi")
    reply <- wait a
    reply @?= Message "bye!"

------------------------------------------------------------------------

testActor1 :: Message -> Actor
testActor1 (Message "inc") = Actor (return (Message "ack"))

testActor2 :: RemoteRef -> Message -> Actor
testActor2 rref msg@(Message "inc") = Actor $ do
  p <- send rref msg
  on p (\(MessageR (Message "ack")) -> modify (add "x" 1))
  return (Message "inced")
testActor2 _rref (Message "sum") = Actor $ do
  s <- get
  return (Message (show (getField "x" s)))

eventLoopB :: String -> EventLoopName
eventLoopB suffix = EventLoopName ("event-loop-actormap-b" ++ "-" ++ suffix)

------------------------------------------------------------------------

unit_actorMapOnAndState :: Assertion
unit_actorMapOnAndState = do
  (time, h) <- fakeTimeEpoch
  reply2 <- catch (do let evA = eventLoopA "onAndState"
                          evB = eventLoopB "onAndState"
                      elA <- makeEventLoop time (makeSeed 0) (NamedPipe "/tmp") evA
                      elB <- makeEventLoop time (makeSeed 0) (NamedPipe "/tmp") evB
                      lref1 <- spawn elA testActor1 emptyState
                      let rref1 = localToRemoteRef evA lref1
                      lref2 <- spawn elB (testActor2 rref1) (stateFromList [("x", Integer 0)])
                      reply <- ainvoke elB lref2 (Message "inc")
                      reply @?= Message "inced"
                      threadDelay 100000
                      reply2 <- ainvoke elB lref2 (Message "sum")
                      quit elA
                      quit elB
                      return reply2)
    (\(e :: SomeException) -> return (Message (show e)))
  reply2 @?= Message "Integer 1"

------------------------------------------------------------------------

testActor3 :: Message -> Actor
testActor3 (Message "go") = Actor $ do
  p <- asyncIO (return (String "io done"))
  on p (\(IOResultR (String "io done")) -> modify (add "x" 1))
  return (Message "done")

unit_actorMapIO :: Assertion
unit_actorMapIO = withEventLoop (eventLoopA "io") $ \el _h -> do
  lref <- spawn el testActor3 (stateFromList [("x", Integer 0)])
  _done <- ainvoke el lref (Message "go")
  threadDelay 100000
  s <- getActorState el lref
  s @?= stateFromList [("x", Integer 1)]

testActor4 :: Message -> Actor
testActor4 (Message "go") = Actor $ do
  p <- asyncIO (error "failed")
  on p (\(ExceptionR _exception) -> modify (add "x" 1))
  return (Message "done")

unit_actorMapIOFail :: Assertion
unit_actorMapIOFail = withEventLoop (eventLoopA "io_fail") $ \el _h -> do
  lref <- spawn el testActor4 (stateFromList [("x", Integer 0)])
  _done <- ainvoke el lref (Message "go")
  threadDelay 100000
  s <- getActorState el lref
  s @?= stateFromList [("x", Integer 1)]

------------------------------------------------------------------------

testActor5 :: RemoteRef -> Message -> Actor
testActor5 rref (Message "go") = Actor $ do
  p <- send rref (Message "hi")
  on p (\TimeoutR -> modify (add "x" 1))
  return (Message "done")

unit_actorMapSendTimeout :: Assertion
unit_actorMapSendTimeout = do
  let ev = eventLoopA "send_timeout"
  withEventLoop ev $ \el h -> do
    let rref = RemoteRef "doesnt_exist" 0
    lref <- spawn el (testActor5 rref) (stateFromList [("x", Integer 0)])
    _done <- ainvoke el lref (Message "go")
    -- Timeout happens after 60 seconds.
    advanceFakeTime h 59
    threadDelay 100000
    s <- getActorState el lref
    s @?= stateFromList [("x", Integer 0)]
    advanceFakeTime h 1
    threadDelay 100000
    s' <- getActorState el lref
    s' @?= stateFromList [("x", Integer 1)]

------------------------------------------------------------------------

testActor6 :: Message -> Actor
testActor6 (Message "go") = Actor $ do
  d <- random
  t <- getTime
  return (Message (show d ++ " " ++ show t))

unit_actorMapRandomAndTime :: Assertion
unit_actorMapRandomAndTime = do
  let ev = eventLoopA "random_and_time"
  withEventLoop ev $ \el h -> do
    lref <- spawn el testActor6 emptyState
    result <- ainvoke el lref (Message "go")
    result @?= Message "0.9871468153391151 1970-01-01 00:00:00 UTC"
    advanceFakeTime h 1
    result2 <- ainvoke el lref (Message "go")
    -- XXX: This is wrong, because seed doesn't get updated...
    result2 @?= Message "0.9871468153391151 1970-01-01 00:00:01 UTC"

testActor7 :: Message -> Actor
testActor7 (Message "go") = Actor $ do
  p <- setTimer 10
  on p (\TimerR -> modify (add "x" 1))
  return (Message "done")

unit_actorMapTimer :: Assertion
unit_actorMapTimer = do
  let ev = eventLoopA "timer"
  withEventLoop ev $ \el h -> do
    lref <- spawn el testActor7 (stateFromList [("x", Integer 0)])
    _done <- ainvoke el lref (Message "go")
    -- Timer happens after 10 seconds.
    advanceFakeTime h 9
    threadDelay 10000
    s <- getActorState el lref
    s @?= stateFromList [("x", Integer 0)]
    advanceFakeTime h 1
    threadDelay 10000
    s' <- getActorState el lref
    s' @?= stateFromList [("x", Integer 1)]
