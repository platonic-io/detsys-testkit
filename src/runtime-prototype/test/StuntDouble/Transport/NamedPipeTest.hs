module StuntDouble.Transport.NamedPipeTest where

import Test.HUnit

import StuntDouble

------------------------------------------------------------------------

unit_transportNamedPipe :: Assertion
unit_transportNamedPipe = do
  t <- namedPipeTransport "/tmp" (EventLoopName "transportNamedPipe")
  let e = Envelope RequestKind (RemoteRef "from" 0) (InternalMessage "msg")
                   (RemoteRef "transportNamedPipe" 1) 0 (LogicalTime (NodeName "x") 0)
  transportSend t e
  me' <- transportReceive t
  me' @?= Just e

unit_transportNamedPipeNothing :: Assertion
unit_transportNamedPipeNothing = do
  t <- namedPipeTransport "/tmp" (EventLoopName "transportNamedPipeNothing")
  me <- transportReceive t
  me @?= Nothing
