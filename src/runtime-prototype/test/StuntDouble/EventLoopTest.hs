{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module StuntDouble.EventLoopTest where

import Control.Exception
import Control.Concurrent
import Control.Concurrent.Async
import Test.HUnit hiding (State)

import StuntDouble

------------------------------------------------------------------------

testActor :: Message -> Actor
testActor (Message "hi") = return (Now (Message "bye!"))

testActor2 :: RemoteRef -> Message -> Actor
testActor2 rref (Message "init") = do
  a <- remoteCall rref (Message "hi")
  return (Later a (\(Message msg) -> return (Now (Message ("Got: " ++ msg)))))

testActor3 :: Message -> Actor
testActor3 (Message "init") = do
  a <- asyncIO (threadDelay 300000 >> return (String "result"))
  return (LaterIO a (\(String result) -> return (Now (Message ("Got: " ++ result)))))

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
  dump el1
  dump el2
  -}

  quit el1
  quit el2

unit_asyncIO :: Assertion
unit_asyncIO = do
  elog <- emptyEventLog
  let ev = eventLoopA "asyncIO"
  el <- makeEventLoop "/tmp" ev elog
  lref <- spawn el testActor3
  a <- send el (localToRemoteRef ev lref) (Message "init")
  reply <- wait a
  reply @?= Message "Got: result"
  quit el
  {-
  catch (do lref <- spawn el testActor3
            a <- send el (localToRemoteRef ev lref) (Message "init")
            reply <- wait a
            reply @?= Message "Got: result")
    (\(e :: SomeException) -> dump el >> eventLog el >>= mapM_ print >> print e)
-}

statefulActor :: Message -> Actor
statefulActor (Message intStr) = do
  s <- get
  let int :: Integer
      int = read intStr
      s' :: State
      s' = add "x" int s
  put s'
  return (Now (Message (show (getState s'))))

unit_state :: Assertion
unit_state = do
  elog <- emptyEventLog
  let ev = eventLoopA "state"
  el <- makeEventLoop "/tmp" ev elog
  lref <- spawn el statefulActor
  reply <- invoke el lref (Message "1")
  reply @?= Message "fromList [(\"x\",Integer 1)]"
  reply2 <- invoke el lref (Message "2")
  reply2 @?= Message "fromList [(\"x\",Integer 3)]"
  quit el
