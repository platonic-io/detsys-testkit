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
import StuntDouble.Reference
import StuntDouble.Time

------------------------------------------------------------------------

withEventLoop :: EventLoopName -> (EventLoop -> IO ()) -> IO ()
withEventLoop name k = do
  (time, h) <- fakeTimeEpoch
  el <- makeEventLoop time (NamedPipe "/tmp") name
  k el
  quit el

eventLoopA :: String -> EventLoopName
eventLoopA suffix = EventLoopName ("event-loop-actormap-a" ++ "-" ++ suffix)

testActor :: Message -> Actor
testActor (Message "hi") = Actor (return (Message "bye!"))

------------------------------------------------------------------------

unit_actorMapInvoke :: Assertion
unit_actorMapInvoke = withEventLoop (eventLoopA "invoke") $ \el -> do
  lref <- spawn el testActor emptyState
  reply <- ainvoke el lref (Message "hi")
  reply @?= Message "bye!"

unit_actorMapSend :: Assertion
unit_actorMapSend = do
  let ev = eventLoopA "send"
  withEventLoop ev $ \el -> do
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
  on p (\(Right (Right (Message "ack"))) -> modify (add "x" 1))
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
                      elA <- makeEventLoop time (NamedPipe "/tmp") evA
                      elB <- makeEventLoop time (NamedPipe "/tmp") evB
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
  on p (\(Right (Left (String "io done"))) -> modify (add "x" 1))
  return (Message "done")

unit_actorMapIO :: Assertion
unit_actorMapIO = withEventLoop (eventLoopA "io") $ \el -> do
  lref <- spawn el testActor3 (stateFromList [("x", Integer 0)])
  _done <- ainvoke el lref (Message "go")
  threadDelay 100000
  s <- getActorState el lref
  s @?= stateFromList [("x", Integer 1)]

testActor4 :: Message -> Actor
testActor4 (Message "go") = Actor $ do
  p <- asyncIO (error "failed")
  on p (\(Left _exception) -> modify (add "x" 1))
  return (Message "done")

unit_actorMapIOFail :: Assertion
unit_actorMapIOFail = withEventLoop (eventLoopA "io_fail") $ \el -> do
  lref <- spawn el testActor4 (stateFromList [("x", Integer 0)])
  _done <- ainvoke el lref (Message "go")
  threadDelay 100000
  s <- getActorState el lref
  s @?= stateFromList [("x", Integer 1)]

-- XXX: timeout tests
