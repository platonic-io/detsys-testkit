module Lec05.ClientGenerator where

import Data.ByteString.Lazy (ByteString)

import Lec05.Event
import Lec05.StateMachine
import Lec05.Time

data NextClientRequest
  = CurrentlyNoRequests
  | Now Event
  | Later Time Event

data ClientGenerator = ClientGenerator
  { cgRespond :: ClientId -> ByteString -> IO ()
  , cgNextClientRequest :: IO NextClientRequest
  }

emptyGenerator :: ClientGenerator
emptyGenerator = ClientGenerator
  { cgRespond = \ _ _ -> return ()
  , cgNextClientRequest = return CurrentlyNoRequests
  }
