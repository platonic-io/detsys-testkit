{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module StuntDouble.ActorMapTest where

import Control.Concurrent
import Control.Concurrent.Async
import Control.Exception
import Test.HUnit hiding (State)

import StuntDouble

------------------------------------------------------------------------

eventLoopA :: String -> EventLoopName
eventLoopA suffix = EventLoopName ("event-loop-actormap-a" ++ "-" ++ suffix)

withEventLoop :: EventLoopName -> (EventLoop -> FakeTimeHandle -> IO ()) -> IO ()
withEventLoop name k = do
  (time, h) <- fakeTimeEpoch
  el <- makeEventLoop time (makeSeed 0) (NamedPipe "/tmp") name
  k el h
  quit el

testActor :: Message -> Actor
testActor (InternalMessage "hi") = Actor (return (InternalMessage "bye!"))

------------------------------------------------------------------------

unit_actorMapInvoke :: Assertion
unit_actorMapInvoke = withEventLoop (eventLoopA "invoke") $ \el _h -> do
  lref <- spawn el testActor emptyState
  reply <- ainvoke el lref (InternalMessage "hi")
  reply @?= InternalMessage "bye!"

unit_actorMapSend :: Assertion
unit_actorMapSend = do
  let ev = eventLoopA "send"
  withEventLoop ev $ \el _h -> do
    lref <- spawn el testActor emptyState
    let rref = localToRemoteRef ev lref
    a <- asend el rref (InternalMessage "hi")
    reply <- wait a
    reply @?= InternalMessage "bye!"

------------------------------------------------------------------------

testActor1 :: Message -> Actor
testActor1 (InternalMessage "inc") = Actor (return (InternalMessage "ack"))

testActor2 :: RemoteRef -> Message -> Actor
testActor2 rref msg@(InternalMessage "inc") = Actor $ do
  p <- send rref msg
  on p (\(InternalMessageR (InternalMessage "ack")) -> modify (add "x" 1))
  return (InternalMessage "inced")
testActor2 _rref (InternalMessage "sum") = Actor $ do
  s <- get
  return (InternalMessage (show (getField "x" s)))

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
                      reply <- ainvoke elB lref2 (InternalMessage "inc")
                      reply @?= InternalMessage "inced"
                      -- XXX: Using waitForEventLoop here instead of sleep seems
                      -- to hang. Also this test is flaky, could it be that
                      -- there's a bug lurking here?
                      threadDelay 500000
                      reply2 <- ainvoke elB lref2 (InternalMessage "sum")
                      quit elA
                      quit elB
                      return reply2)
    (\(e :: SomeException) -> return (InternalMessage (show e)))
  reply2 @?= InternalMessage "Integer 1"

------------------------------------------------------------------------

testActor3 :: Message -> Actor
testActor3 (InternalMessage "go") = Actor $ do
  p <- asyncIO (return (String "io done"))
  on p (\(IOResultR (String "io done")) -> modify (add "x" 1))
  return (InternalMessage "done")

unit_actorMapIO :: Assertion
unit_actorMapIO = withEventLoop (eventLoopA "io") $ \el _h -> do
  lref <- spawn el testActor3 (stateFromList [("x", Integer 0)])
  _done <- ainvoke el lref (InternalMessage "go")
  waitForEventLoop el
  s <- getActorState el lref
  s @?= stateFromList [("x", Integer 1)]

testActor4 :: Message -> Actor
testActor4 (InternalMessage "go") = Actor $ do
  p <- asyncIO (error "failed")
  on p (\(ExceptionR _exception) -> modify (add "x" 1))
  return (InternalMessage "done")

unit_actorMapIOFail :: Assertion
unit_actorMapIOFail = withEventLoop (eventLoopA "io_fail") $ \el _h -> do
  lref <- spawn el testActor4 (stateFromList [("x", Integer 0)])
  _done <- ainvoke el lref (InternalMessage "go")
  waitForEventLoop el
  s <- getActorState el lref
  s @?= stateFromList [("x", Integer 1)]

------------------------------------------------------------------------

testActor5 :: RemoteRef -> Message -> Actor
testActor5 rref (InternalMessage "go") = Actor $ do
  p <- send rref (InternalMessage "hi")
  on p (\TimeoutR -> modify (add "x" 1))
  return (InternalMessage "done")

unit_actorMapSendTimeout :: Assertion
unit_actorMapSendTimeout = do
  let ev = eventLoopA "send_timeout"
  withEventLoop ev $ \el h -> do
    let rref = RemoteRef "doesnt_exist" 0
    lref <- spawn el (testActor5 rref) (stateFromList [("x", Integer 0)])
    _done <- ainvoke el lref (InternalMessage "go")
    -- Timeout happens after 60 seconds.
    advanceFakeTime h 59
    waitForEventLoopModuloTimeouts el
    s <- getActorState el lref
    s @?= stateFromList [("x", Integer 0)]
    advanceFakeTime h 1
    waitForEventLoop el
    s' <- getActorState el lref
    s' @?= stateFromList [("x", Integer 1)]

------------------------------------------------------------------------

testActor6 :: Message -> Actor
testActor6 (InternalMessage "go") = Actor $ do
  d <- random
  t <- getTime
  return (InternalMessage (show d ++ " " ++ show t))

unit_actorMapRandomAndTime :: Assertion
unit_actorMapRandomAndTime = do
  let ev = eventLoopA "random_and_time"
  withEventLoop ev $ \el h -> do
    lref <- spawn el testActor6 emptyState
    result <- ainvoke el lref (InternalMessage "go")
    result @?= InternalMessage "2.323435770446025e-7 1970-01-01 00:00:00 UTC"
    advanceFakeTime h 1
    result2 <- ainvoke el lref (InternalMessage "go")
    result2 @?= InternalMessage "7.94137909876369e-8 1970-01-01 00:00:01 UTC"

testActor7 :: Message -> Actor
testActor7 (InternalMessage "go") = Actor $ do
  p <- setTimer 10
  on p (\TimerR -> modify (add "x" 1))
  return (InternalMessage "done")

unit_actorMapTimer :: Assertion
unit_actorMapTimer = do
  let ev = eventLoopA "timer"
  withEventLoop ev $ \el h -> do
    lref <- spawn el testActor7 (stateFromList [("x", Integer 0)])
    _done <- ainvoke el lref (InternalMessage "go")
    -- Timer happens after 10 seconds.
    advanceFakeTime h 9
    waitForEventLoopModuloTimeouts el
    s <- getActorState el lref
    s @?= stateFromList [("x", Integer 0)]
    advanceFakeTime h 1
    waitForEventLoop el
    s' <- getActorState el lref
    s' @?= stateFromList [("x", Integer 1)]
