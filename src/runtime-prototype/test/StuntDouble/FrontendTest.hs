{-# LANGUAGE OverloadedStrings #-}

module StuntDouble.FrontendTest where

import Control.Concurrent
import Control.Concurrent.Async
import Test.HUnit

import StuntDouble

------------------------------------------------------------------------

summer :: RemoteRef -> RemoteRef -> Message -> Actor
summer left right (ClientRequest "sum" cref) = Actor $ do
  p <- send left  (InternalMessage "get")
  q <- send right (InternalMessage "get")
  let c (InternalMessageR (InternalMessage msg)) = do
        s <- get
        let sum = getField "sum" s
        if sum == Integer 0
        then do
          let Integer i = read msg
          modify (add "sum" i)
        else
          clientResponse cref (InternalMessage (show (plus sum (read msg))))
  on p c
  on q c
  return (InternalMessage "done")

summand :: Message -> Actor
summand (InternalMessage "get") = Actor $ do
  s <- get
  return (InternalMessage (show (getField "x" s)))

withEventLoop :: EventLoopName -> (EventLoop -> FakeTimeHandle -> IO ()) -> IO ()
withEventLoop name k = do
  (time, h) <- fakeTimeEpoch
  el <- makeEventLoop time (makeSeed 0) (NamedPipe "/tmp") name
  k el h
  quit el

eventLoop :: String -> EventLoopName
eventLoop suffix = EventLoopName ("event-loop-http-frontend-" ++ suffix)

unit_httpFrontend :: Assertion
unit_httpFrontend = do
  let ev = eventLoop "httpFrontend"
  withEventLoop ev $ \el _h -> do
    lref1 <- spawn el summand (stateFromList [("x", Integer 1)])
    lref2 <- spawn el summand (stateFromList [("x", Integer 2)])
    lref3 <- spawn el (summer (localToRemoteRef ev lref1)
                              (localToRemoteRef ev lref2))
                              (stateFromList [("sum", Integer 0)])
    let port = 3040
    withHttpFrontend el lref3 port $ \_pid -> do
      -- XXX: Add better waiting mechanism...
      threadDelay 100000

      eResp <- makeClientRequest (InternalMessage "sum") port
      eResp @?= Right (InternalMessage "Integer 3")
