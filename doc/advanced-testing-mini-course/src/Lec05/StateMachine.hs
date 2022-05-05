{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Lec05.StateMachine where

import Data.Fixed
import System.Random
import Data.ByteString.Lazy (ByteString)
import Data.TreeDiff (ToExpr)

import Lec05.Time

------------------------------------------------------------------------

newtype NodeId = NodeId { unNodeId :: Int }
  deriving stock (Eq, Ord)
  deriving newtype (Read, Show, ToExpr)

newtype ClientId = ClientId { unClientId :: Int }
  deriving stock (Eq, Ord)
  deriving newtype (Read, Show, ToExpr)

type SMStep state message response
  = state -> StdGen -> ([Output response message], state, StdGen)

data SM state request message response = SM
  { smState   :: state
  , smStep    :: Input request message -> SMStep state message response
  , smTimeout :: Time -> SMStep state message response
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
  | RegisterTimerSeconds Pico
  | ResetTimerSeconds Pico
  deriving (Eq, Show)

noTimeouts :: Time -> SMStep state message response
noTimeouts _time state stdgen = ([], state, stdgen)

echoSM :: SM () ByteString ByteString ByteString
echoSM = SM
  { smState   = ()
  , smStep    = \(ClientRequest _at cid req) () gen -> ([ClientResponse cid req], (), gen)
  , smTimeout = noTimeouts
  }
