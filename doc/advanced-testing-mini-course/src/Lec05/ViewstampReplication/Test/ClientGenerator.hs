module Lec05.ViewstampReplication.Test.ClientGenerator where

import Lec05.ClientGenerator
import Lec05.Codec
import Lec05.StateMachine

import Lec05.ViewstampReplication.Message

vrClientGenerator :: SingleStateGenerator
vrClientGenerator = SingleStateGenerator
  0
  (+1)
  (\ curRequestNumber ->
     let msg = "msg" ++ show curRequestNumber
     in (NodeId 0, encShow $ VRRequest msg curRequestNumber))
