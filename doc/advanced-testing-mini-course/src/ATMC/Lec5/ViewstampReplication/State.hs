{-# LANGUAGE TemplateHaskell #-}
module ATMC.Lec5.ViewstampReplication.State where

import Data.List (sort)
import Data.Map (Map)
import Data.Sequence (Seq)
import Data.Set (Set)

import ATMC.Lec5.StateMachine
import ATMC.Lec5.StateMachineDSL
import ATMC.Lec5.ViewstampReplication.Message

data Status = Normal | ViewChange | Recovering

-- I think here is where the bug is
data ClientStatus result
  = InFlight  { requestNumber :: RequestNumber
              , copNumber :: OpNumber }
  | Completed { requestNumber :: RequestNumber
              , copNumber :: OpNumber
              , theResult :: result, theViewNumber :: ViewNumber}

type InnerStateMachine state op result = state -> op -> (result, state)

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
  , _theLog :: Seq OpNumber
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
  , _stateMachine :: InnerStateMachine state op result
  }

makeLenses ''VRState

initState :: [NodeId] -> NodeId -> state -> InnerStateMachine state op result
  -> VRState state op result
initState config me state stateInterface = VRState
  { _configuration = topo
  , _replicaNumber = fst $ head $ filter ((== me) . snd) $ zip [0..] topo
  , _currentViewNumber = 0
  , _currentStatus = Normal
  , _opNumber = 0
  , _theLog = mempty
  , _commitNumber = 0 -- ?
  , _clientTable = mempty
  -- not in paper
  , _primaryPrepareOk = mempty
  , _currentState = state
  , _stateMachine = stateInterface
  }
  where
    topo = sort (me:config)
