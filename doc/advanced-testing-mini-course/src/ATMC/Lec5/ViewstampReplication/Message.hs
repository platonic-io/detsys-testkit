{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module ATMC.Lec5.ViewstampReplication.Message where

import ATMC.Lec5.StateMachine

newtype RequestNumber = RequestNumber Int
  deriving newtype Num
newtype ViewNumber = ViewNumber Int
  deriving newtype Num
newtype OpNumber = OpNumber Int
  deriving newtype Num
newtype CommitNumber = CommitNumber Int
  deriving newtype Num

data VRRequest op
  = VRRequest op RequestNumber -- ClientId in `ClientRequest`

type Result = ()

data VRResponse
  = VRReply ViewNumber RequestNumber Result

data VRMessage op
  = Prepare ViewNumber op OpNumber CommitNumber
  | PrepareOk ViewNumber OpNumber {- i which is node-id -}
  | Commit ViewNumber CommitNumber
