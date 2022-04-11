module ATMC.Lec5.ViewstampReplication.Machine where

import ATMC.Lec5.StateMachine
import ATMC.Lec5.StateMachineDSL
import ATMC.Lec5.ViewstampReplication.Message
import ATMC.Lec5.ViewstampReplication.State


type VROp = () -- ?
type VR a = SMM VRState (VRMessage VROp) VRResponse a

tODO :: a
tODO = error "Not implemented yet"

machine :: Input (VRRequest VROp) (VRMessage VROp) -> VR ()
machine = tODO

sm :: [NodeId] -> NodeId -> SM VRState (VRRequest VROp) (VRMessage VROp) VRResponse
sm otherNodes me = SM (initState otherNodes me) (runSMM 0 . machine)
