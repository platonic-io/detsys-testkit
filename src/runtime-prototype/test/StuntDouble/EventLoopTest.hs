{-# LANGUAGE ScopedTypeVariables #-}

module StuntDouble.EventLoopTest where

import Control.Exception
import Control.Concurrent.Async
import Test.HUnit

import StuntDouble

------------------------------------------------------------------------

testActor :: Message -> Actor
testActor (Message "hi") = return (Now (Message "bye!"))

testActor2 :: RemoteRef -> Message -> Actor
testActor2 rref (Message "init") = do
  a <- remoteCall rref (Message "hi")
  return (Later a (\(Message msg) -> return (Now (Message ("Got: " ++ msg)))))

eventLoopA :: EventLoopName
eventLoopA = EventLoopName "event-loop-a"

eventLoopB :: EventLoopName
eventLoopB = EventLoopName "event-loop-b"

------------------------------------------------------------------------

unit_invoke :: Assertion
unit_invoke = do
  elog <- emptyEventLog
  el <- makeEventLoop "/tmp" eventLoopA elog
  lref <- spawn el testActor
  reply <- invoke el lref (Message "hi")
  reply @?= Message "bye!"
  l <- fmap (filter (not . isComment)) (eventLog el)
  quit el
  l @?=
    [LogInvoke (dummyDeveloperRef el) lref (Message "hi") (Message "bye!") eventLoopA]

unit_send :: Assertion
unit_send = do
  elog <- emptyEventLog
  el <- makeEventLoop "/tmp" eventLoopA elog
  catch
    (do lref <- spawn el testActor
        let rref = localToRemoteRef (EventLoopName "event-loop-a") lref
        a <- send el rref (Message "hi")
        reply <- wait a
        reply @?= Message "bye!"
        l <- fmap (filter (not . isComment)) (eventLog el)
        quit el
        l @?=
          [ LogSendStart (dummyDeveloperRef el) rref (Message "hi") 0 eventLoopA
          , LogRequest (dummyDeveloperRef el) rref (Message "hi") (Message "bye!") eventLoopA
          , LogReceive rref (dummyDeveloperRef el) (Message "bye!") 0 eventLoopA
          , LogSendFinish (CorrelationId 0) (Message "bye!") eventLoopA
          ])
    (\(e :: SomeException) -> dump el >> eventLog el >>= mapM_ print)

unit_sendLater :: Assertion
unit_sendLater = do
  elog <- emptyEventLog
  el1 <- makeEventLoop "/tmp" eventLoopA elog
  el2 <- makeEventLoop "/tmp" eventLoopB elog

  lref1 <- spawn el1 testActor
  lref2 <- spawn el2 (testActor2 (localToRemoteRef eventLoopA lref1))
  a <- send el2 (localToRemoteRef eventLoopB lref2) (Message "init")
  reply <- wait a
  reply @?= Message "Got: bye!"

  -- catch
  --   (do lref1 <- spawn el1 testActor
  --       lref2 <- spawn el2 (testActor2 (localToRemoteRef eventLoopA lref1))
  --       a <- send el2 (localToRemoteRef eventLoopB lref2) (Message "init")
  --       reply <- wait a
  --       reply @?= Message "ot: bye!")
  --   (\(e :: SomeException) -> dump el1 >> dump el2 >> eventLog el1 >>= mapM_ print)

  dump el1
  dump el2

  quit el1
  quit el2
