{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

module StuntDouble.ActorMapTest where

import Control.Exception
import Control.Concurrent
import Control.Concurrent.Async
import Test.HUnit hiding (State)

import StuntDouble.Actor.State
import StuntDouble.ActorMap
import StuntDouble.Datatype
import StuntDouble.EventLoop.Transport
import StuntDouble.Message
import StuntDouble.FreeMonad
import StuntDouble.Reference

------------------------------------------------------------------------

eventLoopA :: String -> EventLoopName
eventLoopA suffix = EventLoopName ("event-loop-actormap-a" ++ "-" ++ suffix)

testActor :: Message -> Actor
testActor (Message "hi") = Actor (return (Message "bye!"))

------------------------------------------------------------------------

unit_actorMapInvoke :: Assertion
unit_actorMapInvoke = do
  let ev = eventLoopA "invoke"
  el <- makeEventLoop (NamedPipe "/tmp") ev
  lref <- spawn el testActor emptyState
  reply <- ainvoke el lref (Message "hi")
  reply @?= Message "bye!"
  quit el

unit_actorMapSend :: Assertion
unit_actorMapSend = do
  let ev = eventLoopA "send"
  el <- makeEventLoop (NamedPipe "/tmp") ev
  lref <- spawn el testActor emptyState
  let rref = localToRemoteRef ev lref
  a <- asend el rref (Message "hi")
  reply <- wait a
  reply @?= Message "bye!"
  quit el

------------------------------------------------------------------------

testActor1 :: Message -> Actor
testActor1 (Message "inc") = Actor (return (Message "ack"))

testActor2 :: RemoteRef -> Message -> Actor
testActor2 rref msg@(Message "inc") = Actor $ do
  p <- send rref msg
  on p (\(Right (Message "ack")) -> do
           s <- get
           put (add "x" 1 s))
  return (Message "inced")
testActor2 _rref (Message "sum") = Actor $ do
  s <- get
  return (Message (show (getField "x" s)))

eventLoopB :: String -> EventLoopName
eventLoopB suffix = EventLoopName ("event-loop-actormap-b" ++ "-" ++ suffix)

------------------------------------------------------------------------

unit_actorMapOnAndState :: Assertion
unit_actorMapOnAndState = do
  reply2 <- catch (do let evA = eventLoopA "onAndState"
                          evB = eventLoopB "onAndState"
                      elA <- makeEventLoop (NamedPipe "/tmp") evA
                      elB <- makeEventLoop (NamedPipe "/tmp") evB
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
