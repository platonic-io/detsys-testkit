{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module StuntDouble.ActorMapTest where

import Control.Concurrent.Async
import Control.Exception
import Test.HUnit hiding (State)

import StuntDouble

------------------------------------------------------------------------

eventLoopA :: String -> EventLoopName
eventLoopA suffix = EventLoopName ("event-loop-actormap-a" ++ "-" ++ suffix)

testActor :: Message -> Actor ()
testActor (InternalMessage "hi") = Actor (return (InternalMessage "bye!"))

------------------------------------------------------------------------

unit_actorMapInvoke :: Assertion
unit_actorMapInvoke = withEventLoop (eventLoopA "invoke") $ \el _h -> do
  lref <- spawn el testActor ()
  reply <- ainvoke el lref (InternalMessage "hi")
  reply @?= InternalMessage "bye!"

unit_actorMapSend :: Assertion
unit_actorMapSend = do
  let ev = eventLoopA "send"
  withEventLoop ev $ \el _h -> do
    lref <- spawn el testActor ()
    let rref = localToRemoteRef ev lref
    a <- asend el rref (InternalMessage "hi")
    reply <- wait a
    reply @?= InternalMessage "bye!"

------------------------------------------------------------------------

testActor1 :: Message -> Actor ()
testActor1 (InternalMessage "inc") = Actor (return (InternalMessage "ack"))

testActor2 :: RemoteRef -> Message -> Actor Int
testActor2 rref msg@(InternalMessage "inc") = Actor $ do
  p <- send rref msg
  on p (\(InternalMessageR (InternalMessage "ack")) -> modify succ)
  return (InternalMessage "inced")
testActor2 _rref (InternalMessage "sum") = Actor $ do
  s <- get
  return (InternalMessage (show s))

eventLoopB :: String -> EventLoopName
eventLoopB suffix = EventLoopName ("event-loop-actormap-b" ++ "-" ++ suffix)

------------------------------------------------------------------------

unit_actorMapOnAndState :: Assertion
unit_actorMapOnAndState = do
  (time, h) <- fakeClockEpoch
  reply2 <- catch (do let evA = eventLoopA "onAndState"
                          evB = eventLoopB "onAndState"
                      elA <- makeEventLoop time (makeSeed 0) (NamedPipe "/tmp")
                               (AdminNamedPipe "/tmp") fakeCodec FakeDisk evA
                      elB <- makeEventLoop time (makeSeed 0) (NamedPipe "/tmp")
                               (AdminNamedPipe "/tmp") fakeCodec FakeDisk evB
                      lref1 <- spawn elA testActor1 ()
                      let rref1 = localToRemoteRef evA lref1
                      lref2 <- spawn elB (testActor2 rref1) 0
                      reply <- ainvoke elB lref2 (InternalMessage "inc")
                      reply @?= InternalMessage "inced"
                      waitForEventLoop elA
                      waitForEventLoopModuloTimeouts elB
                      reply2 <- ainvoke elB lref2 (InternalMessage "sum")
                      quit elA
                      quit elB
                      return reply2)
    (\(e :: SomeException) -> return (InternalMessage (show e)))
  reply2 @?= InternalMessage "1"

------------------------------------------------------------------------

testActor3 :: Message -> Actor Int
testActor3 (InternalMessage "go") = Actor $ do
  p <- asyncIO (IOReturn (IOString "io done"))
  on p (\(IOResultR (IOString "io done")) -> modify succ)
  return (InternalMessage "done")

unit_actorMapIO :: Assertion
unit_actorMapIO = withEventLoop (eventLoopA "io") $ \el _h -> do
  lref <- spawn el testActor3 0
  _done <- ainvoke el lref (InternalMessage "go")
  waitForEventLoop el
  s <- getActorState el lref
  s @?= (1 :: Int)

testActor4 :: Message -> Actor Int
testActor4 (InternalMessage "go") = Actor $ do
  p <- asyncIO (error "failed")
  on p (\(ExceptionR _exception) -> modify succ)
  return (InternalMessage "done")

unit_actorMapIOFail :: Assertion
unit_actorMapIOFail = withEventLoop (eventLoopA "ioFail") $ \el _h -> do
  lref <- spawn el testActor4 0
  _done <- ainvoke el lref (InternalMessage "go")
  waitForEventLoop el
  s <- getActorState el lref
  s @?= (1 :: Int)

------------------------------------------------------------------------

testActor5 :: RemoteRef -> Message -> Actor Int
testActor5 rref (InternalMessage "go") = Actor $ do
  p <- send rref (InternalMessage "hi")
  on p (\TimeoutR -> modify succ)
  return (InternalMessage "done")

unit_actorMapSendTimeout :: Assertion
unit_actorMapSendTimeout = do
  let ev = eventLoopA "sendTimeout"
  withEventLoop ev $ \el h -> do
    let rref = RemoteRef "doesntExist" 0
    lref <- spawn el (testActor5 rref) 0
    _done <- ainvoke el lref (InternalMessage "go")
    -- Timeout happens after 60 seconds.
    advanceFakeClock h 59
    -- XXX: Step all handlers explicitly here to ensure that timeout doesn't happen.
    s <- getActorState el lref
    s @?= (0 :: Int)
    advanceFakeClock h 1
    waitForEventLoop el
    s' <- getActorState el lref
    s' @?= (1 :: Int)

------------------------------------------------------------------------

testActor6 :: Message -> Actor s
testActor6 (InternalMessage "go") = Actor $ do
  d <- random
  t <- getTime
  return (InternalMessage (show d ++ " " ++ show t))

unit_actorMapRandomAndTime :: Assertion
unit_actorMapRandomAndTime = do
  let ev = eventLoopA "randomAndTime"
  withEventLoop ev $ \el h -> do
    lref <- spawn el testActor6 ()
    result <- ainvoke el lref (InternalMessage "go")
    result @?= InternalMessage "RandomInterval 2.323435770446025e-7 Time 1970-01-01 00:00:00 UTC"
    advanceFakeClock h 1
    result2 <- ainvoke el lref (InternalMessage "go")
    result2 @?= InternalMessage "RandomInterval 7.94137909876369e-8 Time 1970-01-01 00:00:01 UTC"

testActor7 :: Message -> Actor Int
testActor7 (InternalMessage "go") = Actor $ do
  p <- setTimer 10
  on p (\TimerR -> modify succ)
  return (InternalMessage "done")

unit_actorMapTimer :: Assertion
unit_actorMapTimer = do
  let ev = eventLoopA "timer"
  withEventLoop ev $ \el h -> do
    lref <- spawn el testActor7 0
    _done <- ainvoke el lref (InternalMessage "go")
    -- Timer happens after 10 seconds.
    advanceFakeClock h 9
    -- XXX: Step all handlers explicitly here to ensure that timeout doesn't happen.
    s <- getActorState el lref
    s @?= (0 :: Int)
    advanceFakeClock h 1
    waitForEventLoop el
    s' <- getActorState el lref
    s' @?= (1 :: Int)
