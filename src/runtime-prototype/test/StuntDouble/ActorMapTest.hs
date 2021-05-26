{-# LANGUAGE ScopedTypeVariables #-}

module StuntDouble.ActorMapTest where

import Control.Exception
import Control.Concurrent.Async
import Test.HUnit hiding (State)

import StuntDouble.Actor.State
import StuntDouble.ActorMap
import StuntDouble.EventLoop.Transport
import StuntDouble.Message
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
  reply <- invoke el lref (Message "hi")
  reply @?= Message "bye!"
  quit el

unit_actorMapSend :: Assertion
unit_actorMapSend = do
  let ev = eventLoopA "send"
  el <- makeEventLoop (NamedPipe "/tmp") ev
  lref <- spawn el testActor emptyState
  let rref = localToRemoteRef ev lref
  a <- send el rref (Message "hi")
  reply <- wait a
  reply @?= Message "bye!"
  quit el
