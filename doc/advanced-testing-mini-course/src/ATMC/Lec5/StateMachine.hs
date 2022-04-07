module ATMC.Lec5.StateMachine where

import Data.ByteString.Lazy (ByteString)

import ATMC.Lec5.Time

------------------------------------------------------------------------

newtype NodeId = NodeId Int
  deriving (Eq, Ord, Show)

newtype ClientId = ClientId Int
  deriving (Eq, Ord, Show)

data SM state request message response = SM
  { smState :: state
  , smStep  :: Input request message -> state -> ([Output response message], state)
  }

data Input request message
  = ClientRequest Time ClientId request
  | InternalMessage Time NodeId message

data Output response message
  = ClientResponse ClientId response
  | InternalMessageOut NodeId message
  deriving (Eq, Show)

inputReceiver :: RawInput -> NodeId
inputReceiver (RawInput to (ClientRequest   _at _from _req)) = to
inputReceiver (RawInput to (InternalMessage _at _from _msg)) = to

data RawInput = RawInput NodeId (Input ByteString ByteString)
