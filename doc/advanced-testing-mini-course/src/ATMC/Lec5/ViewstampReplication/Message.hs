{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
module ATMC.Lec5.ViewstampReplication.Message where

import Data.Sequence (Seq)
import qualified Data.Sequence as S

import ATMC.Lec5.StateMachine
import ATMC.Lec5.StateMachineDSL

newtype RequestNumber = RequestNumber Int
  deriving newtype (Eq, Num, Ord, Read, Show)
newtype ViewNumber = ViewNumber Int
  deriving newtype (Eq, Num, Ord, Read, Show)
newtype OpNumber = OpNumber Int
  deriving newtype (Enum, Eq, Ord, Num, Read, Show)
newtype CommitNumber = CommitNumber Int
  deriving newtype (Enum, Eq, Num, Read, Show)
newtype Nonce = Nonce Int
  deriving newtype (Read, Show)
newtype Log op = Log (Seq op)
  deriving newtype (Monoid, Read, Semigroup, Show)

data VRRequest op
  = VRRequest op RequestNumber -- ClientId in `ClientRequest`
  deriving (Eq, Read, Show)

data VRResponse result
  = VRReply ViewNumber RequestNumber result
  deriving (Eq, Read, Show)

data InternalClientMessage op = InternalClientMessage
  { _operation :: op
  , _clientId :: ClientId
  , _clientRequestNumber :: RequestNumber
  }
  deriving (Read, Show)

makeLenses ''InternalClientMessage

data VRMessage op
  -- 4.1 Normal Operation
  = Prepare ViewNumber (InternalClientMessage op) OpNumber CommitNumber
  | PrepareOk ViewNumber OpNumber {- i which is node-id -}
  | Commit ViewNumber CommitNumber
  -- 4.3 Recovery
  | Recovery Nonce {- i which is node-id -}
  | RecoveryResponse ViewNumber Nonce (Log op) OpNumber CommitNumber
  deriving (Read, Show)

(|>) :: Log op -> op -> Log op
(Log l) |> o = Log (l S.|> o)

logLookup :: OpNumber -> Log op -> Maybe op
logLookup (OpNumber i) (Log l) = S.lookup (pred i) l
