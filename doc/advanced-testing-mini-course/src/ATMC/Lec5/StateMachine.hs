module ATMC.Lec5.StateMachine where

import Data.ByteString.Lazy (ByteString)

import ATMC.Lec5.Time

------------------------------------------------------------------------

newtype NodeId = NodeId { unNodeId :: Int }
  deriving (Eq, Ord, Show)

newtype ClientId = ClientId { unClientId :: Int }
  deriving (Eq, Ord, Show)

data SM state request message response = SM
  { smState :: state
  , smStep  :: Input request message -> state -> ([Output response message], state)
  -- smPredicate :: state -> [pred]
  -- smProcess :: pred -> state -> ([Output response message], state)
  }

data Input request message
  = ClientRequest Time ClientId request
  | InternalMessage Time NodeId message
  deriving Show

data Output response message
  = ClientResponse ClientId response
  | InternalMessageOut NodeId message
  deriving (Eq, Show)

echoSM :: SM () ByteString ByteString ByteString
echoSM = SM
  { smState = ()
  , smStep  = \(ClientRequest _at cid req) () -> ([ClientResponse cid req], ())
  }
