{-# LANGUAGE OverloadedStrings #-}
module Debugger.Main where

import qualified Data.Vector as Vector

import Debugger.UI (AppState, mkAppState, runApp)
import Debugger.State

------------------------------------------------------------------------

fakeState :: AppState
fakeState = mkAppState (Vector.fromList [f1, f2,f1,f1,f2,f2,f3,f1,f1,f2,f2,f2,f1,f1,f1,f2])
  where
    e1 = DebEvent
      { from = "A"
      , to = "B"
      , event = "fst"
      , receivedLogical = 0
      , message = "This is the first message"
      }
    e2 = DebEvent
      { from = "B"
      , to = "A"
      , event = "snd"
      , receivedLogical = 1
      , message = "This is the second message"
      }
    f1 = InstanceState
      { state = "state of A"
      , currentEvent = e1
      , seqDia = "sequence diagram from 0"
      , logs = ["first log of A", "second log of A"]
      , sent = [e2]
      }
    f2 = InstanceState
      { state = "state of B"
      , currentEvent = e2
      , seqDia = "sequence diagram from 1"
      , logs = ["Log for B"]
      , sent = []
      }
    f3 = InstanceState
      { state = "state of B"
      , currentEvent = e2
      , seqDia = "sequence diagram from 1"
      , logs = ["Log for B"]
      , sent = [e1,e2,e1,e1,e1]
      }

debugMain :: IO ()
debugMain = runApp fakeState
