{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module ATMC.Lec5.SmartBFT.Messages where

import ATMC.Lec5.StateMachine
import ATMC.Lec5.Time (Time)

type TransactionContent = String
type TransactionId = Int
type Hash = String

data SBFTRequest
  = ClientAppend TransactionContent -- in SmartBFT it is [TransactionContent]
  | Get TransactionId

newtype RoundId = RoundId Int
  deriving newtype (Enum, Eq, Ord)
newtype RegencyId = RegencyId Int
type Source = NodeId

data Proposal = Proposal
  { pTransactions :: [TransactionContent]
  , pTimeStamp :: Time
  }

data ConsensusMessage
  = Propose
      Hash
      Proposal
  | Write
      Hash
  | Accept
      Hash

data LeaderElectionMessage
  = Stop
  | StopData -- should sign a proof, * Proof of decision and quorum
  | Sync RoundId -- also sends proofs, but we don't have crypto

data StateTransferMessage
  = ReadyTargetRequest
  | ReadyTargetResponse
      { stmLeader :: Source
      , stmCID :: RoundId
      , stmRID :: RegencyId
      }
  | HashRequest
      RoundId
  | HashResponse
      RoundId
      [Hash]
      Bool -- complete?
  | DataRequest
      Hash
      RoundId
  | DataResponse
      (Either String [TransactionContent]) -- can be error

data SBFTMessage
  = ConsensusMessage
      { cmCID :: RoundId
      , cmRID :: RegencyId
      , cmSource :: Source
      , smCM :: ConsensusMessage
      }
  | Append
      Source
      [TransactionContent]
  | Forward
      Source
      [TransactionContent]
  | LeaderElectionMessage
      { leSource :: Source
      , leRID :: RegencyId
      , leLEM :: LeaderElectionMessage
      }
  | StateTransferMessage
      { stMID :: String -- not sure what this is
      , stSource :: Source
      , stSTM :: StateTransferMessage
      }
