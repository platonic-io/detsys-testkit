module StuntDouble.Transport.StmTest where

import Test.HUnit

import StuntDouble

------------------------------------------------------------------------

unit_stmTransport :: IO ()
unit_stmTransport = do
  t <- stmTransport
  let e = Envelope RequestKind (RemoteRef "from" 0) (InternalMessage "msg")
                   (RemoteRef "a" 1) 0 (LogicalTime (NodeName "x") 0)
  transportSend t e
  e' <- transportReceive t
  e' @?= Just e
