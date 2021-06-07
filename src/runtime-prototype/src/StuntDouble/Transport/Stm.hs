module StuntDouble.Transport.Stm where

import Control.Concurrent.STM

import StuntDouble.Transport

------------------------------------------------------------------------

stmTransport :: IO (Transport IO)
stmTransport = do
  chan <- newTChanIO
  return Transport
    { transportSend = \e -> atomically (writeTChan chan e)
    , transportReceive = atomically (readTChan chan)
    }
