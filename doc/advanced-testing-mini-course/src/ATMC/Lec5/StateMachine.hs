module ATMC.Lec5.StateMachine where

import Data.ByteString.Lazy (ByteString)

import ATMC.Lec5.Time

------------------------------------------------------------------------

newtype NodeId = NodeId Int
newtype ClientId = ClientId Int

data SM state request message response = SM
  { smState :: state
  , smStep  :: Input request message -> state -> ([Output response message], state)
  }

data Input request message
  = ClientRequest Time ClientId NodeId request
  | InternalMessage Time NodeId NodeId message

newtype RawInput = RawInput (Input ByteString ByteString)

data Output response message
  = ClientResponse ClientId response
  | InternalMessageOut NodeId message
