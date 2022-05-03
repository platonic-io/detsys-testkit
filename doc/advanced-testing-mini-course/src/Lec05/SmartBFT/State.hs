{-# LANGUAGE TemplateHaskell #-}

module Lec05.SmartBFT.State where

import Data.Map (Map)
import Data.Sequence (Seq)
import Data.Set (Set)
import Data.Time (NominalDiffTime)

import Lec05.StateMachine
import Lec05.StateMachineDSL
import Lec05.Time (Time)
import Lec05.SmartBFT.Messages

data SBFTState = SBFTState
  { _globalState :: GlobalState
  , _consensusState :: ConsensusState
  , _leadershipState :: LeadershipState
  , _stateTransferState :: StateTransferState
  , _pendingQueue :: PendingQueue
  , _theLog :: Seq TransactionContent
  }

data GlobalState = GlobalState
  { _me :: NodeId
  , _membership :: [NodeId]
  , _height :: RoundId
  , _regency :: RegencyId
  , _leader :: Source
  , _pendingRegency :: RegencyId
  , _electionsEnabled :: Bool
  , _stateTransferInProgress :: Bool
  }


--------------------------------------------------------------------------------
-- Consenus
--------------------------------------------------------------------------------

data ConsensusState = ConsensusState
  { _rounds :: Map RoundId Round
  , _rebroadCastInterval :: NominalDiffTime
  , _lastInitiatedRound :: RoundId
  , _consensusPaused :: Bool
  }

data Round = Round
  { rCID :: RoundId
  , rVotes :: Map RegencyId ConsensusVotes
  , rDecided :: Bool
  , rDecidedRegency :: RegencyId
  , rDecision :: Maybe Decision
  , rLastProposeTime :: Maybe Time
  , rLastWriteTime :: Maybe Time
  , rLastAcceptTime :: Maybe Time
  }

data Decision = Decision RoundId Hash Proposal

data VoteStore = VoteStore
  { vsVotes :: Map Source Hash
  , vsCounts :: Map String Int
  , vsMax :: Maybe Hash
  }

data ConsensusVotes = ConsensusVotes
  { cvPropose :: Maybe Proposal
  , cvWrites :: VoteStore
  , cvAccepts :: VoteStore
  }


--------------------------------------------------------------------------------
-- LeaderElection
--------------------------------------------------------------------------------

data LeadershipState = LeadershipState
  { lsLocalStop :: Maybe Time
  , lsLocalStopData :: Maybe (Source, Time)
  , lsVotes :: Map RegencyId RegencyState
  , lsEarly :: [SBFTMessage] -- only LeaderElectionMessage
  , lsNestedTimerStarted :: Time
  , lsRebroadCastInterval :: NominalDiffTime
  , lsNestedElectionInterval :: NominalDiffTime
  }

data RegencyState = RegencyState
  { rsRID :: RegencyId
  , rsStopVote :: Set Source
  , rsStodDatas :: Set Source
  , rsSynced :: Bool
  }

--------------------------------------------------------------------------------
-- StateTransfer
--------------------------------------------------------------------------------

-- TODO implement
data StateTransferState = StateTranferState

--------------------------------------------------------------------------------
-- PendingQueue
--------------------------------------------------------------------------------

-- TODO implement
data PendingQueue = PendingQueue

makeLenses ''SBFTState

makeLenses ''GlobalState
makeLenses ''ConsensusState
