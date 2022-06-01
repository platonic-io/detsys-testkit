{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
module Lec05.ViewstampReplication.State where

import Data.Fixed
import Data.List (sort)
import Data.Map (Map)
import Data.Set (Set)
import Data.TreeDiff (ToExpr(toExpr), Expr(App))
import GHC.Generics (Generic)

import Lec05.StateMachine
import Lec05.StateMachineDSL
import Lec05.ViewstampReplication.Message

data Status = Normal | ViewChange | Recovering
  deriving (Generic, Eq, Show)

instance ToExpr Status

-- I think here is where the bug is
data ClientStatus result
  = InFlight  { requestNumber :: RequestNumber
              , copNumber :: OpNumber }
  | Completed { requestNumber :: RequestNumber
              , copNumber :: OpNumber
              , theResult :: result, theViewNumber :: ViewNumber}
  deriving (Generic, Show)

instance ToExpr result => ToExpr (ClientStatus result)

newtype ReplicatedStateMachine state op result = ReplicatedStateMachine {runReplicated :: state -> op -> (result, state)}
  deriving Generic

instance Show (ReplicatedStateMachine s o r) where
  show _ = "RSM"

instance ToExpr (ReplicatedStateMachine s o r) where
  toExpr _ = App "RSM" []

data VRState state op result = VRState
  { _configuration :: [NodeId]
  -- ^ The configuration. This is a sorted array containing
  --the IP addresses of each of the 2f + 1 replicas.
  , _replicaNumber :: Int -- me == configuration !! replicaNumber
  -- ^ The replica number. This is the index into the con-
  -- figuration where this replica’s IP address is stored.
  , _currentViewNumber :: ViewNumber
  -- ^ The current view-number, initially 0.
  , _currentStatus :: Status
  -- ^ The current status, either normal, view-change, or
  -- recovering.
  , _opNumber :: OpNumber
  -- ^ The op-number assigned to the most recently re-
  -- ceived request, initially 0.
  , _theLog :: Log op
  -- ^ The log. This is an array containing op-number
  -- entries. The entries contain the requests that have
  -- been received so far in their assigned order.
  , _commitNumber :: CommitNumber
  -- ^ The commit-number is the op-number of the most
  -- recently committed operation.
  , _clientTable :: Map ClientId (ClientStatus result)
  -- ^ The client-table. This records for each client the
  -- number of its most recent request, plus, if the re-
  -- quest has been executed, the result sent for that re-
  -- quest.

  -- not in paper
  , _primaryPrepareOk :: Map OpNumber (Set NodeId)
  -- so the actual state machine is not listed
  , _currentState :: state
  , _stateMachine :: ReplicatedStateMachine state op result
  -- view change not listed
  , _startViewChangeResponses :: Map ViewNumber (Set NodeId)
  , _doViewChangeResponses :: Map ViewNumber (Set NodeId, Log op, ViewNumber, OpNumber, CommitNumber)
  -- state transfer not listed
  , _broadCastInterval :: Pico
  , _recoveryResponses :: Map Nonce (Set NodeId)
  , _currentNonce :: Maybe Nonce -- should we store this in the `Status`?
  , _primaryResponse :: Maybe (PrimaryRecoveryResponse op)
  }
  deriving (Generic, Show)

instance (ToExpr state, ToExpr op, ToExpr result) => ToExpr (VRState state op result)

makeLenses ''VRState

initState :: [NodeId] -> NodeId -> Pico -> state -> ReplicatedStateMachine state op result
  -> VRState state op result
initState config me theBroadCastInterval state stateInterface = VRState
  { _configuration = topo
  , _replicaNumber = fst $ head $ filter ((== me) . snd) $ zip [0..] topo
  , _currentViewNumber = 0
  , _currentStatus = Normal
  , _opNumber = 0
  , _theLog = mempty
  , _commitNumber = -1 -- ?
  , _clientTable = mempty
  -- not in paper
  , _primaryPrepareOk = mempty
  , _currentState = state
  , _stateMachine = stateInterface
  -- not in paper view change
  , _startViewChangeResponses = mempty
  , _doViewChangeResponses = mempty
  -- not in paper state transfer
  , _broadCastInterval = theBroadCastInterval
  , _recoveryResponses = mempty
  , _currentNonce = Nothing
  , _primaryResponse = Nothing
  }
  where
    topo = sort (me:config)
