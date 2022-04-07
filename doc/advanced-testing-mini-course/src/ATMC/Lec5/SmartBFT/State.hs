{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module ATMC.Lec5.SmartBFT.State where

import Data.Map (Map)
import Data.Sequence (Seq)
import Data.Set (Set)
import Data.Time (NominalDiffTime)
import GHC.Records.Compat

import ATMC.Lec5.StateMachine
import ATMC.Lec5.Time (Time)
import ATMC.Lec5.SmartBFT.Messages

data SBFTState = SBFTState
  { globalState :: GlobalState
  , consensusState :: ConsensusState
  , leadershipState :: LeadershipState
  , stateTransferState :: StateTransferState
  , pendingQueue :: PendingQueue
  , theLog :: Seq TransactionContent
  }

data GlobalState = GlobalState
  { me :: NodeId
  , membership :: [NodeId]
  , height :: RoundId
  , regency :: RegencyId
  , leader :: Source
  , pendingRegency :: RegencyId
  , electionsEnabled :: Bool
  , stateTransferInProgress :: Bool
  }

--------------------------------------------------------------------------------
-- Consenus
--------------------------------------------------------------------------------

data ConsensusState = ConsensusState
  { rounds :: Map RoundId Round
  , rebroadCastInterval :: NominalDiffTime
  , lastInitiatedRound :: RoundId
  , consensusPaused :: Bool
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

--------------------------------------------------------------------------------
-- Fields...
--------------------------------------------------------------------------------

instance HasField "leader" SBFTState NodeId where
  hasField s = (\x -> s {globalState = (globalState s) {leader = x}}, leader (globalState s))

instance HasField "me" SBFTState NodeId where
  hasField s = (\x -> s {globalState = (globalState s) {me = x}}, me (globalState s))

instance HasField "height" SBFTState RoundId where
  hasField s = (\x -> s {globalState = (globalState s) {height = x}}, height (globalState s))

instance HasField "regency" SBFTState RegencyId where
  hasField s = (\x -> s {globalState = (globalState s) {regency = x}}, regency (globalState s))

instance HasField "stateTransferInProgress" SBFTState Bool where
  hasField s = (\x -> s {globalState = (globalState s) {stateTransferInProgress = x}}, stateTransferInProgress (globalState s))

instance HasField "consensusPaused" SBFTState Bool where
  hasField s = (\x -> s {consensusState = (consensusState s) {consensusPaused = x}}, consensusPaused (consensusState s))

instance HasField "lastInitiatedRound" SBFTState RoundId where
  hasField s = (\x -> s {consensusState = (consensusState s) {lastInitiatedRound = x}}, lastInitiatedRound (consensusState s))
