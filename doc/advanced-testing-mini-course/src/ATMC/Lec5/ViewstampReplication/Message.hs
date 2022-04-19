{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
module ATMC.Lec5.ViewstampReplication.Message where

import ATMC.Lec5.StateMachine
import ATMC.Lec5.StateMachineDSL

newtype RequestNumber = RequestNumber Int
  deriving newtype (Eq, Num, Ord)
newtype ViewNumber = ViewNumber Int
  deriving newtype (Eq, Num)
newtype OpNumber = OpNumber Int
  deriving newtype (Enum, Eq, Ord, Num)
newtype CommitNumber = CommitNumber Int
  deriving newtype Num

data VRRequest op
  = VRRequest op RequestNumber -- ClientId in `ClientRequest`

data VRResponse result
  = VRReply ViewNumber RequestNumber result

data InternalClientMessage op = InternalClientMessage
  { _operation :: op
  , _clientId :: ClientId
  , _clientRequestNumber :: RequestNumber
  }

makeLenses ''InternalClientMessage

data VRMessage op
  = Prepare ViewNumber (InternalClientMessage op) OpNumber CommitNumber
  | PrepareOk ViewNumber OpNumber {- i which is node-id -}
  | Commit ViewNumber CommitNumber
