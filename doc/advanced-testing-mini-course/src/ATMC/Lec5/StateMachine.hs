module ATMC.Lec5.StateMachine where

import Data.ByteString.Lazy (ByteString)

import ATMC.Lec5.Time

------------------------------------------------------------------------

newtype NodeId = NodeId Int
  deriving Show

newtype ClientId = ClientId Int
  deriving Show

data SM state request message response = SM
  { smState :: state
  , smStep  :: Input request message -> state -> ([Output response message], state)
  }

data Input request message
  = ClientRequest Time ClientId NodeId request
  | InternalMessage Time NodeId NodeId message

data Output response message
  = ClientResponse ClientId response
  | InternalMessageOut NodeId message
  deriving Show

inputReceiver :: RawInput -> NodeId
inputReceiver (RawInput (ClientRequest   _at _from to _req)) = to
inputReceiver (RawInput (InternalMessage _at _from to _msg)) = to

newtype RawInput = RawInput (Input ByteString ByteString)
