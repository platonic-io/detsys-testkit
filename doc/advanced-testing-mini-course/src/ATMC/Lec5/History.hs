{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE StandaloneDeriving #-}

module ATMC.Lec5.History where

import Control.Concurrent.STM
import Control.Concurrent.STM.TQueue

import ATMC.Lec5.StateMachine

------------------------------------------------------------------------

newtype History = History (TQueue HistEvent)

data HistEvent = forall state req msg resp.
  (Show state, Show req, Show msg, Show resp) =>
  HistEvent NodeId state (Input req msg) state [Output resp msg]
deriving instance Show HistEvent

newHistory :: IO History
newHistory = do
  q <- newTQueueIO
  return (History q)

appendHistory :: History -> HistEvent -> IO ()
appendHistory (History q) ev = atomically (writeTQueue q ev)

readHistory :: History -> IO [HistEvent]
readHistory (History q) = atomically (flushTQueue q)
