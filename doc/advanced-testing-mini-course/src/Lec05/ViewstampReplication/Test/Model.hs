module Lec05.ViewstampReplication.Test.Model where

import qualified Lec04.LineariseWithFault as Lec4

import Lec05.ViewstampReplication.State (ReplicatedStateMachine(..))
import Lec05.ViewstampReplication.Message

type Model = [String]
type Command = VRRequest String
type Response = VRResponse [String]

step :: Model -> Command -> (Model, Response)
step xs (VRRequest op rn) = (op:xs, VRReply 0 rn (op:xs))

initModel :: [String]
initModel = []

smI :: ReplicatedStateMachine [String] String [String]
smI = ReplicatedStateMachine $ \ s o -> (o:s, o:s)

---

markFailure :: Lec4.History' Command Response -> Lec4.History' Command Response
markFailure (Lec4.History ops) = Lec4.History (finishClients [] $ map go ops)
  where
    go i@Lec4.Invoke{} = i
    go f@Lec4.Fail{} = f
    go (Lec4.Ok p VROnlyOneInflightAllowed{}) = Lec4.Fail p Lec4.FAIL
    go o@Lec4.Ok{} = o

    remove x = filter (/= x)

    finishClients ps [] = [ Lec4.Fail p Lec4.FAIL | p <- ps]
    finishClients ps (op:h) = case op of
      Lec4.Invoke p _ -> op : finishClients (p:ps) h
      Lec4.Ok p _ -> op : finishClients (remove p ps) h
      Lec4.Fail p _ -> op : finishClients (remove p ps) h
