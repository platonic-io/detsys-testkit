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

eventLoopA :: String -> EventLoopName
eventLoopA suffix = EventLoopName ("event-loop-a" ++ "-" ++ suffix)

eventLoopB :: String -> EventLoopName
eventLoopB suffix = EventLoopName ("event-loop-b" ++ "-" ++ suffix)

------------------------------------------------------------------------

unit_invoke :: Assertion
unit_invoke = do
  elog <- emptyEventLog
  let ev = eventLoopA "invoke"
  el <- makeEventLoop "/tmp" ev elog
  lref <- spawn el testActor
  reply <- invoke el lref (Message "hi")
  reply @?= Message "bye!"
  l <- fmap (filter (not . isComment)) (eventLog el)
  quit el
  l @?=
    [LogInvoke (dummyDeveloperRef el) lref (Message "hi") (Message "bye!") ev]

unit_send :: Assertion
unit_send = do
  elog <- emptyEventLog
  let ev = eventLoopA "send"
  el <- makeEventLoop "/tmp" ev elog
  catch
    (do lref <- spawn el testActor
        let rref = localToRemoteRef ev lref
        a <- send el rref (Message "hi")
        reply <- wait a
        reply @?= Message "bye!"
        l <- fmap (filter (not . isComment)) (eventLog el)
        quit el
        l @?=
          [ LogSendStart (dummyDeveloperRef el) rref (Message "hi") 0 ev
          , LogRequest (dummyDeveloperRef el) rref (Message "hi") (Message "bye!") ev
          , LogReceive rref (dummyDeveloperRef el) (Message "bye!") 0 ev
          , LogSendFinish (CorrelationId 0) (Message "bye!") ev
          ])
    (\(e :: SomeException) -> dump el >> eventLog el >>= mapM_ print)

unit_sendLater :: Assertion
unit_sendLater = do
  elog <- emptyEventLog
  let evA = eventLoopA "sendLater"
      evB = eventLoopB "sendLater"
  el1 <- makeEventLoop "/tmp" evA elog
  el2 <- makeEventLoop "/tmp" evB elog

  lref1 <- spawn el1 testActor
  lref2 <- spawn el2 (testActor2 (localToRemoteRef evA lref1))
  a <- send el2 (localToRemoteRef evB lref2) (Message "init")
  reply <- wait a
  reply @?= Message "Got: bye!"

  {-
  catch
    (do lref1 <- spawn el1 testActor
        lref2 <- spawn el2 (testActor2 (localToRemoteRef eventLoopA lref1))
        a <- send el2 (localToRemoteRef eventLoopB lref2) (Message "init")
        reply <- wait a
        reply @?= Message "Got: bye!")
    (\(e :: SomeException) -> dump el1 >> dump el2 >> eventLog el1 >>= mapM_ print)
  -}

  dump el1
  dump el2

  quit el1
  quit el2
