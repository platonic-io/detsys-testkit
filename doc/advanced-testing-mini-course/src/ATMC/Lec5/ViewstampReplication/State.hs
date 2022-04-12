{-# LANGUAGE TemplateHaskell #-}
module ATMC.Lec5.ViewstampReplication.State where

import Data.List (sort)
import Data.Map (Map)
import Data.Sequence (Seq)

import ATMC.Lec5.StateMachine
import ATMC.Lec5.StateMachineDSL
import ATMC.Lec5.ViewstampReplication.Message

data Status = Normal | ViewChange | Recovering

-- I think here is where the bug is
data ClientStatus
  = InFlight  {requestNumber :: RequestNumber}
  | Completed {requestNumber :: RequestNumber, theResult :: Result, theViewNumber :: ViewNumber}

data VRState = VRState
  { _configuration :: [NodeId]
  , _replicaNumber :: Int -- me == configuration !! replicaNumber
  , _currentViewNumber :: ViewNumber
  , _currentStatus :: Status
  , _opNumber :: OpNumber
  , _theLog :: Seq OpNumber
  , _commitNumber :: CommitNumber -- highe
  , _clientTable :: Map ClientId ClientStatus
  }

makeLenses ''VRState

initState :: [NodeId] -> NodeId -> VRState
initState config me = VRState
  { _configuration = topo
  , _replicaNumber = fst $ head $ filter ((== me) . snd) $ zip [0..] topo
  , _currentViewNumber = 0
  , _currentStatus = Normal
  , _opNumber = 0
  , _theLog = mempty
  , _commitNumber = 0 -- ?
  , _clientTable = mempty
  }
  where
    topo = sort (me:config)
