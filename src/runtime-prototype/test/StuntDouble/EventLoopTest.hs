{-# LANGUAGE ScopedTypeVariables #-}

module StuntDouble.EventLoopTest where

import Control.Exception
import Control.Concurrent.Async
import Test.HUnit

import StuntDouble

------------------------------------------------------------------------

unit_invoke :: Assertion
unit_invoke = do
  el <- makeEventLoop "/tmp" eventLoopA
  lref <- spawn el testActor
  reply <- invoke el lref (Message "hi")
  reply @?= Message "bye!"
  l <- eventLog el
  l @?=
    [LogInvoke (dummyDeveloperRef el) lref (Message "hi") (Message "bye!") eventLoopA]

unit_send :: Assertion
unit_send = do
  el <- makeEventLoop "/tmp" eventLoopA
  lref <- spawn el testActor
  let rref = localToRemoteRef (EventLoopName "event-loop-a") lref
  a <- send el rref (Message "hi")
  reply <- wait a
  reply @?= Message "bye!"
  l <- eventLog el
  l @?=
    [ LogSendStart (dummyDeveloperRef el) rref (Message "hi") 0 eventLoopA
    , LogRequest (dummyDeveloperRef el) rref (Message "hi") (Message "bye!") eventLoopA
    , LogSendFinish (CorrelationId 0) (Message "bye!") eventLoopA
    ]
