module StuntDouble.Transport.StmTest where

import Control.Concurrent.Async
import Test.HUnit

import StuntDouble

------------------------------------------------------------------------

unit_stmTransport :: IO ()
unit_stmTransport = do
  t <- stmTransport
  let e = Envelope RequestKind (RemoteRef "from" 0) (InternalMessage "msg")
                   (RemoteRef "a" 1) 0
  a <- transportSend t e
  e' <- transportReceive t
  e' @?= Just e
