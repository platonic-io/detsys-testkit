module StuntDouble.Transport.NamedPipeTest where

import Control.Concurrent.Async
import Test.HUnit

import StuntDouble

------------------------------------------------------------------------

unit_sendReceive :: IO ()
unit_sendReceive = do
  t <- namedPipeTransport "/tmp" (EventLoopName "a")
  let e = Envelope RequestKind (RemoteRef "from" 0) (InternalMessage "msg")
                   (RemoteRef "a" 1) 0
  a <- async (transportSend t e)
  e' <- transportReceive t
  cancel a
  e' @?= e
