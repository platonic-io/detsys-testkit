module StuntDouble.Transport.NamedPipeTest where

import Test.HUnit

import StuntDouble

------------------------------------------------------------------------

unit_transportNamedPipe :: IO ()
unit_transportNamedPipe = do
  t <- namedPipeTransport "/tmp" (EventLoopName "a")
  let e = Envelope RequestKind (RemoteRef "from" 0) (InternalMessage "msg")
                   (RemoteRef "a" 1) 0
  transportSend t e
  e' <- transportReceive t
  e' @?= Just e
