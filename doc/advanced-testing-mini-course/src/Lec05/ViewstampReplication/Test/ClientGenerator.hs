module Lec05.ViewstampReplication.Test.ClientGenerator where

import Data.ByteString.Lazy (ByteString)

import Lec05.ClientGenerator
import Lec05.Codec
import Lec05.StateMachine
import Lec05.Time

import Lec05.ViewstampReplication.Message

data VRClientState = VRClientState
  { curRequestNumber :: RequestNumber
  , curPrimary :: NodeId -- currently we never update, but in the future the client might learn new leader
  }

initVR :: VRClientState
initVR = VRClientState 0 (NodeId 0)

nextVR :: VRClientState -> VRClientState
nextVR (VRClientState rn l) = VRClientState (rn+1) l

genVR :: ClientId -> VRClientState -> (NodeId, ByteString)
genVR (ClientId c) (VRClientState rn l) =
  let
    msg = "msg" ++ show c ++ "-" ++ show rn
  in (l, encShow $ VRRequest msg rn)

vrClientGenerator :: SingleStateGenerator
vrClientGenerator = SingleStateGenerator initVR nextVR genVR

vrClientDelay :: NominalDiffTime
vrClientDelay = 2

vrGeneratorSchema :: GeneratorSchema
vrGeneratorSchema = Multiple vrClientGenerator vrClientDelay 2
