{-# LANGUAGE MultiWayIf #-}
module Lec05.ViewstampReplication.Machine where

import Control.Monad (forM_, unless, when)
import Data.Fixed
import qualified Data.Map as Map
import qualified Data.Set as Set
import GHC.Stack (HasCallStack)

import Lec05.Agenda
import Lec05.Codec
import Lec05.StateMachine
import Lec05.StateMachineDSL
import Lec05.Time (Time)
import Lec05.ViewstampReplication.Message
import Lec05.ViewstampReplication.State

{-
Following the `Viewstamped Replication Revisited`
  by Barbara Liskov and James Cowling
https://pmg.csail.mit.edu/papers/vr-revisited.pdf
-}

type VR s o r a = SMM (VRState s o r) (VRMessage o) (VRResponse r) a

tODO :: HasCallStack => a
tODO = error "Not implemented yet"

primaryAt :: ViewNumber -> VR s o r NodeId
primaryAt (ViewNumber cVn) = do
  nodes <- use configuration
  return $ nodes !! (cVn `mod` length nodes)

isPrimaryAt :: ViewNumber -> VR s o r Bool
isPrimaryAt (ViewNumber cVn) = do
  nodes <- use (configuration.to length)
  rn <- use replicaNumber
  return (rn == cVn `mod` nodes)

isPrimary :: VR s o r Bool
isPrimary = use currentViewNumber >>= isPrimaryAt

checkIsPrimary :: VR s o r ()
checkIsPrimary = guardM isPrimary

checkIsNewPrimary :: ViewNumber -> VR s o r ()
checkIsNewPrimary v = guardM (isPrimaryAt v)

checkIsBackup :: ViewNumber -> VR s o r ()
checkIsBackup msgVN = do
  guardM (not <$> isPrimary)
  {-
Replicas only process normal protocol mes-
sages containing a view-number that matches the view-
number they know. If the sender is behind, the receiver
drops the message. If the sender is ahead, the replica
performs a state transfer§
  -}
  cVn <- use currentViewNumber
  if
    | cVn == msgVN -> return ()
    | cVn >  msgVN -> ereturn -- drop the message
    | otherwise {- cVn <  msgVN -} -> initStateTransfer

broadCastReplicas :: VRMessage o -> VR s o r ()
broadCastReplicas msg = do
  nodes <- use configuration
  ViewNumber cVn <- use currentViewNumber
  forM_ (zip [0..] nodes) $ \ (i, node) -> do
    unless (i == cVn `mod` length nodes) $ send node msg

broadCastOtherReplicas :: VRMessage o -> VR s o r ()
broadCastOtherReplicas msg = do
  nodes <- use configuration
  me <- use replicaNumber
  forM_ (zip [0..] nodes) $ \ (i, node) -> do
    unless (i == me) $ send node msg

sendPrimary :: VRMessage o -> VR s o r ()
sendPrimary msg = do
  nodes <- use configuration
  ViewNumber cVn <- use currentViewNumber
  send (nodes !! (cVn `mod` length nodes)) msg

addPrepareOk :: OpNumber -> NodeId -> VR s o r Int
addPrepareOk n i = do
  s <- primaryPrepareOk.at n <%= Just . maybe (Set.singleton i) (Set.insert i)
  return (maybe 0 Set.size s)

isQuorum :: Int -> VR s o r Bool
isQuorum x = do
  n <- use (configuration.to length)
  -- `f` is the largest number s.t 2f+1<=n
  let f = (n - 1) `div` 2
  return (f <= x)

addStartViewChange :: ViewNumber -> NodeId -> VR s o r Int
addStartViewChange v from = do
  s <- startViewChangeResponses.at v <%= Just . maybe (Set.singleton from) (Set.insert from)
  return (maybe 0 Set.size s)

{- 4.2 View Change
3.  it sets its view-number
to that in the messages and selects as the new log
the one contained in the message with the largest
v′; if several messages have the same v′ it selects
the one among them with the largest n.
-}
addDoViewChange :: ViewNumber -> NodeId -> Log o -> ViewNumber -> OpNumber -> CommitNumber
  -> VR s o r (Int, Log o, OpNumber, CommitNumber)
addDoViewChange v from l v' n k = do
  let
    first = (Set.singleton from, l, v', n, k)
    upd (nodes, cl, cv', cn, ck) =
      let
        keepOld = (Set.insert from nodes, cl, cv', cn, ck)
        keepNew = (Set.insert from nodes, l, v', n, k)
      in if
        | cv' <  v' -> keepNew
        | cv' == v' && n > cn -> keepNew
        | otherwise -> keepOld
  m <- doViewChangeResponses.at v <%= Just . maybe first upd
  case m of
    Nothing -> error "IMPOSSIBLE"
    Just (s, hl, _, hn, hk) -> return (Set.size s, hl, hn, hk)

addRecoveryResponse :: Nonce -> NodeId -> VR s o r Int
addRecoveryResponse x from = do
  s <- recoveryResponses.at x <%= Just . maybe (Set.singleton from) (Set.insert from)
  return (maybe 0 Set.size s)

pickLatest :: PrimaryRecoveryResponse op -> PrimaryRecoveryResponse op -> PrimaryRecoveryResponse op
pickLatest p@(PrimaryRecoveryResponse _l _o k) p'@(PrimaryRecoveryResponse _l' _o' k')
  | k <= k' = p'
  | otherwise = p

findClientInfoForOp :: OpNumber -> VR s o r (ClientId, RequestNumber)
findClientInfoForOp on = use $
  clientTable
  .to (Map.filter (\cs -> copNumber cs == on))
  .to Map.findMin
  .to (fmap requestNumber)

initViewChange :: VR s o r ()
initViewChange = do
  {- 4.2 View Change
1. A replica i that notices the need for a view change
advances its view-number, sets its status to view-
change, and sends a 〈STARTVIEWCHANGE v, i〉
message to the all other replicas, where v iden-
tifies the new view. A replica notices the need
for a view change either based on its own timer,
or because it receives a STARTVIEWCHANGE or
DOVIEWCHANGE message for a view with a larger
number than its own view-number.
-}
  v <- currentViewNumber <%= (+1)
  currentStatus .= ViewChange
  broadCastOtherReplicas $ StartViewChange v

generateNonce :: VR s o r Nonce
generateNonce = Nonce <$> random

initStateTransfer :: VR s o r ()
initStateTransfer = do
  {- 4.3 Recovery
1. The recovering replica, i, sends a 〈RECOVERY i, x〉
message to all other replicas, where x is a nonce.
-}
  guardM $ use (currentStatus.to (== Normal))
  nonce <- generateNonce
  currentStatus .= Recovering
  currentNonce  .= Just nonce
  broadCastOtherReplicas $ Recovery nonce
  ereturn

executeReplicatedMachine :: o -> VR s o r r
executeReplicatedMachine op = do
  cs <- use currentState
  rsm <- use stateMachine
  let (result, cs') = runReplicated rsm cs op
  currentState .= cs'
  return result

executeUpToOrBeginStateTransfer :: ViewNumber -> CommitNumber -> VR s o r ()
executeUpToOrBeginStateTransfer _ (-1) = do
  -- -1 means nothing has been comitted yet!
  return ()
executeUpToOrBeginStateTransfer v k = do
  myK <- use commitNumber
  guard $ myK <= k
  l <- use theLog
  forM_ [succ myK .. k] $ \(CommitNumber x) -> do
    let o = OpNumber x
    case logLookup o l of
      Nothing -> initStateTransfer
      Just op -> do
        result <- executeReplicatedMachine op
        (theClientId, theRequestNumber) <- findClientInfoForOp o
        clientTable.at theClientId .= Just (Completed theRequestNumber o result v)
  commitNumber .= k

executePrimary :: ViewNumber -> OpNumber -> VR s o r ()
executePrimary v (OpNumber commitTo) = do
  CommitNumber myK <- use commitNumber
  guard $ myK <= commitTo
  l <- use theLog
  forM_ [succ myK .. commitTo] $ \k -> do
    let o = OpNumber k
    case logLookup o l of
      Nothing -> do
        -- shouldn't happen, we get a confirmation but don't remember the op
        error "Trying to commit operation not in log"
      Just op -> do
        -- TODO: maybe we can't execute this one yet, or we can execute more
        result <- executeReplicatedMachine op
        (theClientId, theRequestNumber) <- findClientInfoForOp o
        clientTable.at theClientId .= Just (Completed theRequestNumber o result v)
        -- TODO: should we gc primaryPrepareOk?
        respond theClientId (VRReply v theRequestNumber result)
  commitNumber .= CommitNumber commitTo

{- 4.1 Normal Operation
The protocol description assumes all participating
replicas are in the same view. Every message sent from
one replica to another contains the sender’s current view-
number. Replicas only process normal protocol mes-
sages containing a view-number that matches the view-
number they know. If the sender is behind, the receiver
drops the message. If the sender is ahead, the replica
performs a state transfer: it requests information it is
missing from the other replicas and uses this information
to bring itself up to date before processing the message.
State transfer is discussed further in Section 5.2
-}
-- TODO: unclear if this only applies to 4.1 messages or others
checkViewNumberOfMessage :: ViewNumber -> VR s o r ()
checkViewNumberOfMessage v = do
  mVn <- use currentViewNumber
  guard (mVn <= v)
  if mVn < v
    then initStateTransfer
    else return ()

machine :: Input (VRRequest o) (VRMessage o) -> VR s o r ()
machine (ClientRequest _time c (VRRequest op s)) = do
  {- 4.1 Normal Operation
1. The client sends a 〈REQUEST op, c, s〉 message to
the primary, where op is the operation (with its ar-
guments) the client wants to run, c is the client-id,
and s is the request-number assigned to the request.
  -}
  checkIsPrimary
  {- 4.1 Normal Operation
2. When the primary receives the request, it compares
the request-number in the request with the informa-
tion in the client table. If the request-number s isn’t
bigger than the information in the table it drops the
request, but it will re-send the response if the re-
quest is the most recent one from this client and it
has already been executed.
  -}
  clientStatus <- use (clientTable.at c)
  case clientStatus of
    Nothing -> return ()
    Just cs -> do
      guard (requestNumber cs <= s)
      case cs of
        Completed s' _v r vn
          | s' == s -> do
              respond c (VRReply vn s r)
              ereturn
          | s < s' -> do
              respond c (VRRequestNumberTooLow s s')
              ereturn
          | otherwise -> return ()
        InFlight _s' _ -> do
          {- 4 The VR Protocol
In addition the client records its own client-id and a
current request-number. A client is allowed to have just
one outstanding request at a time.
-}
          respond c VROnlyOneInflightAllowed
          ereturn
  {- 4.1 Normal Operation
3. The primary advances op-number, adds the request
to the end of the log, and updates the information
for this client in the client-table to contain the new
request number, s. Then it sends a 〈PREPARE v, m,
n, k〉 message to the other replicas, where v is the
current view-number, m is the message it received
from the client, n is the op-number it assigned to
the request, and k is the commit-number.
  -}
  cOp <- use opNumber
  opNumber += 1
  theLog %= (|> op)
  clientTable.at c .= Just (InFlight s cOp)
  v <- use currentViewNumber
  k <- use commitNumber
  broadCastReplicas $ Prepare v (InternalClientMessage op c s) cOp k
  resetTimerSeconds 0 =<< use broadCastInterval
machine (InternalMessage _time from iMsg) = case iMsg of
  Prepare v m n k -> do
    checkViewNumberOfMessage v
    checkIsBackup v
    resetTimerSeconds 0 =<< use broadCastInterval
    {- 4.1 Normal Operation
4. Backups process PREPARE messages in order: a
backup won’t accept a prepare with op-number n
until it has entries for all earlier requests in its log.
When a backup i receives a PREPARE message, it
waits until it has entries in its log for all earlier re-
quests (doing state transfer if necessary to get the
missing information). Then it increments its op-
number, adds the request to the end of its log, up-
dates the client’s information in the client-table, and
sends a 〈PREPAREOK v, n, i〉 message to the pri-
mary to indicate that this operation and all earlier
ones have prepared locally.
    -}
    myOp <- use opNumber
    when (succ myOp /= n) ereturn -- we only process messages in order
    executeUpToOrBeginStateTransfer v k
    opNumber .= n
    theLog %= (|> (m^.operation))
    clientTable.at (m^.clientId) .= Just (InFlight (m^.clientRequestNumber) n)
    sendPrimary $ PrepareOk v n {- i -} -- we don't need to add i since
                                        -- event-loop will add it automatically
  PrepareOk v n -> do
    let i = from
    checkViewNumberOfMessage v
    checkIsPrimary
    {- 4.1 Normal Operation
5. The primary waits for f PREPAREOK messages
from different backups; at this point it considers
the operation (and all earlier ones) to be commit-
ted. Then, after it has executed all earlier operations
(those assigned smaller op-numbers), the primary
executes the operation by making an up-call to the
service code, and increments its commit-number.
Then it sends a 〈REPLY v, s, x〉 message to the
client; here v is the view-number, s is the number
the client provided in the request, and x is the result
of the up-call. The primary also updates the client’s
entry in the client-table to contain the result.
    -}
    howMany <- addPrepareOk n i
    isQ <- isQuorum howMany
    if isQ
      then do
        executePrimary v n
      else ereturn
  Commit v k -> do
    checkViewNumberOfMessage v
    checkIsBackup v
    resetTimerSeconds 0 =<< use broadCastInterval
    {- 4.1 Normal Operation
7. When a backup learns of a commit, it waits un-
til it has the request in its log (which may require
state transfer) and until it has executed all earlier
operations. Then it executes the operation by per-
forming the up-call to the service code, increments
its commit-number, updates the client’s entry in the
client-table, but does not send the reply to the client.
    -}
    executeUpToOrBeginStateTransfer v k
  StartViewChange v -> do
    let i = from
    {- 4.2 View Change
2. When replica i receives STARTVIEWCHANGE mes-
sages for its view-number from f other replicas, it
sends a 〈DOVIEWCHANGE v, l, v’, n, k, i〉 message
to the node that will be the primary in the new view.
Here v is its view-number, l is its log, v′ is the view
number of the latest view in which its status was
normal, n is the op-number, and k is the commit-
number.
-}
    howMany <- addStartViewChange v i
    isQ <- isQuorum howMany -- use same quorum or different?
    if isQ
      then do
        replica <- primaryAt v -- this could be ourself, and that is intentional
        l <- use theLog
        let v' = v -- TODO this should be the viewnumber for last normal..
        n <- use opNumber
        k <- use commitNumber
        send replica $ DoViewChange v l v' n k
      else do
        -- maybe we haven't changed status yet?
        st <- use currentStatus
        if st == Normal
          then initViewChange
          else ereturn
  DoViewChange v l v' n k -> do
    let i = from
    checkViewNumberOfMessage v'
    checkIsNewPrimary v
    guardM $ use (currentStatus.to (== ViewChange))
    {- 4.2 View Change
3. When the new primary receives f + 1
DOVIEWCHANGE messages from different
replicas (including itself), it sets its view-number
to that in the messages and selects as the new log
the one contained in the message with the largest
v′; if several messages have the same v′ it selects
the one among them with the largest n. It sets its
op-number to that of the topmost entry in the new
log, sets its commit-number to the largest such
number it received in the DOVIEWCHANGE mes-
sages, changes its status to normal, and informs the
other replicas of the completion of the view change
by sending 〈STARTVIEW v, l, n, k〉 messages to
the other replicas, where l is the new log, n is the
op-number, and k is the commit-number.
-}
    {- 4.2 View Change
4. The new primary starts accepting client requests. It
also executes (in order) any committed operations
that it hadn’t executed previously, updates its client
table, and sends the replies to the clients.
-}
    (howMany, highestLog, highestN, highestCommit) <- addDoViewChange v i l v' n k
    isQ <- isQuorum howMany
    if isQ
      then do
        currentStatus .= Normal
        currentViewNumber .= v
        theLog .= highestLog
        opNumber .= highestN
        broadCastReplicas $ StartView v highestLog highestN highestCommit
        executePrimary v $ let CommitNumber x = highestCommit in OpNumber x
      else ereturn
  StartView v l n k -> do
    mVn <- use currentViewNumber
    guard (mVn <= v)
    {- 4.2 View Change
5. When other replicas receive the STARTVIEW mes-
sage, they replace their log with the one in the mes-
sage, set their op-number to that of the latest entry
in the log, set their view-number to the view num-
ber in the message, change their status to normal,
and update the information in their client-table. If
there are non-committed operations in the log, they
send a 〈PREPAREOK v, n, i〉 message to the primary;
here n is the op-number. Then they execute all op-
erations known to be committed that they haven’t
executed previously, advance their commit-number,
and update the information in their client-table.
-}
    theLog .= l
    opNumber .= n
    currentViewNumber .= v
    currentStatus .= Normal
    executeUpToOrBeginStateTransfer v k
  Recovery x -> do
    let i = from
    {- 4.3 Recovery
2. A replica j replies to a RECOVERY message only
when its status is normal. In this case the replica
sends a 〈RECOVERYRESPONSE v, x, l, n, k, j〉 mes-
sage to the recovering replica, where v is its view-
number and x is the nonce in the RECOVERY mes-
sage. If j is the primary of its view, l is its log, n is
its op-number, and k is the commit-number; other-
wise these values are nil.
    -}
    guardM $ use (currentStatus.to (== Normal))
    v <- use currentViewNumber
    j <- use replicaNumber
    isP <- isPrimary
    resp <- case isP of
      True -> FromPrimary <$> (PrimaryRecoveryResponse
        <$> {- l-} use theLog
        <*> {- n -} use opNumber
        <*> {- k -} use commitNumber)
      False -> pure FromReplica
    send i $ RecoveryResponse v x resp j
  RecoveryResponse v x resp _j -> do
    checkViewNumberOfMessage v -- TODO: unclear if this should be done
    {- 4.3 Recovery
3. The recovering replica waits to receive at least f +
1 RECOVERYRESPONSE messages from different
replicas, all containing the nonce it sent in its RE-
COVERY message, including one from the primary
of the latest view it learns of in these messages.
Then it updates its state using the information from
the primary, changes its status to normal, and the
recovery protocol is complete.
-}
    guardM $ use (currentStatus.to (== Recovering))
    guardM $ use (currentNonce .to (== Just x))
    -- TODO: We are not checking viewnumber here! Probably should do that
    howMany <- addRecoveryResponse x from
    case resp of
      FromReplica -> return ()
      FromPrimary p -> primaryResponse %= maybe (Just p) (Just . pickLatest p)
    q <- isQuorum howMany
    when q $ do
      mresp <- use primaryResponse
      case mresp of
        Nothing -> return ()
        Just (PrimaryRecoveryResponse l n k) -> do
          theLog .= l
          opNumber .= n
          currentStatus .= Normal
          -- annoying to reset these
          recoveryResponses .= mempty
          currentNonce .= Nothing
          primaryResponse .= Nothing
          executeUpToOrBeginStateTransfer v k

machineInit :: VR s o r ()
machineInit = do
  registerTimerSeconds 0 =<< use broadCastInterval

primaryTock :: Time -> VR s o r ()
primaryTock _t = do
  v <- use currentViewNumber
  OpNumber o <- use opNumber
  k <- use commitNumber
  -- we should only broadcast `Commit` if we haven't seen new client request
  -- see 4.1 Normal Operation, 6)
  -- actually we should resend `Prepare` see 4.1 Normal Operation before algorithm
  guard $ CommitNumber o == k
  broadCastReplicas $ Commit v k
  registerTimerSeconds 0 =<< use broadCastInterval

replicaTock :: Time -> VR s o r ()
replicaTock _t = do
  -- we should arrive here if we haven't gotten `Prepare` or `Commit` in
  -- a while
  guardM $ use (currentStatus.to (== Normal))
  initViewChange

{- 4.1 Normal Operation -- TODO: When we add ticks --
6. Normally the primary informs backups about the
commit when it sends the next PREPARE message;
this is the purpose of the commit-number in the
PREPARE message. However, if the primary does
not receive a new client request in a timely way, it
instead informs the backups of the latest commit by
sending them a 〈COMMIT v, k〉 message, where k
is commit-number (note that in this case commit-
number = op-number).
-}
machineTime :: Time -> VR s o r ()
machineTime currentTime = do
  isP <- isPrimary
  if isP
    then primaryTock currentTime
    else replicaTock currentTime

vrSM :: [NodeId] -> NodeId -> Pico
  -> s -> ReplicatedStateMachine s o r
  -> SM (VRState s o r) (VRRequest o) (VRMessage o) (VRResponse r)
vrSM otherNodes me rbInterval iState iSM = SM
  (initState otherNodes me rbInterval iState iSM)
  (runSMM machineInit)
  (runSMM . machine)
  (runSMM . machineTime)

--------------------------------------------------------------------------------
-- For testing
--------------------------------------------------------------------------------

vrCodec :: (Read m, Show m, Show r) => Codec (VRRequest m) (VRMessage m) (VRResponse r)
vrCodec = showReadCodec

agenda :: Time -> Agenda
agenda endTime = makeEventAgenda endTime []
