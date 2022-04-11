module ATMC.Lec5.ViewstampReplication.State where

import Data.List (sort)
import Data.Map (Map)
import Data.Sequence (Seq)

import ATMC.Lec5.StateMachine
import ATMC.Lec5.ViewstampReplication.Message

data Status = Normal | ViewChange | Recovering

-- I think here is where the bug is
data ClientStatus
  = InFlight RequestNumber
  | Completed RequestNumber Result

data VRState = VRState
  { configuration :: [NodeId]
  , replicaNumber :: Int -- me == configuration !! replicaNumber
  , currentViewNumber :: ViewNumber
  , currentStatus :: Status
  , opNumber :: OpNumber
  , theLog :: Seq OpNumber
  , commitNumber :: CommitNumber -- highest opnumber
  , clientTable :: Map ClientId ClientStatus
  }

initState :: [NodeId] -> NodeId -> VRState
initState config me = VRState
  { configuration = topo
  , replicaNumber = fst $ head $ filter ((== me) . snd) $ zip [0..] topo
  , currentViewNumber = 0
  , currentStatus = Normal
  , opNumber = 0
  , theLog = mempty
  , commitNumber = 0 -- ?
  , clientTable = mempty
  }
  where
    topo = sort (me:config)
