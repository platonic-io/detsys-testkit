{-# LANGUAGE OverloadedStrings #-}

module StuntDouble.FrontendTest where

import Control.Concurrent
import Control.Concurrent.Async
import Network.HTTP.Client
import Test.HUnit

import StuntDouble

------------------------------------------------------------------------

summer :: RemoteRef -> RemoteRef -> Message -> Actor Int
summer left right (ClientRequest' "sum" [] cref) = Actor $ do
  p <- send left  (InternalMessage "get")
  q <- send right (InternalMessage "get")
  let c (InternalMessageR (InternalMessage msg)) = do
        s <- get
        if s == 0
        then do
          let i :: Int
              i = read msg
          modify (i +)
        else
          clientResponse cref (InternalMessage (show (s + read msg)))
  on p c
  on q c
  return (InternalMessage "done")

summand :: Message -> Actor Int
summand (InternalMessage "get") = Actor $ do
  s <- get
  return (InternalMessage (show s))

eventLoop :: String -> EventLoopName
eventLoop suffix = EventLoopName ("event-loop-http-frontend-" ++ suffix)

unit_httpFrontend :: Assertion
unit_httpFrontend = do
  let ev = eventLoop "httpFrontend"
  mgr <- newManager defaultManagerSettings
  withEventLoop ev $ \el _h -> do
    lref1 <- spawn el summand 1
    lref2 <- spawn el summand 2
    lref3 <- spawn el (summer (localToRemoteRef ev lref1)
                              (localToRemoteRef ev lref2))
                              0
    let port = 3040
    eResp <- withHttpFrontend el lref3 port $ \_a -> do
      makeClientRequest mgr (InternalMessage "sum") port
    eResp @?= Right (InternalMessage "3")
