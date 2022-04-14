module ATMC.Lec5.ViewstampReplication.Machine where

import Control.Monad (forM_, unless, when)
import Data.Sequence ((|>))
import qualified Data.Set as Set

import ATMC.Lec5.StateMachine
import ATMC.Lec5.StateMachineDSL
import ATMC.Lec5.ViewstampReplication.Message
import ATMC.Lec5.ViewstampReplication.State

{-
Following the `Viewstamped Replication Revisited`
  by Barbara Liskov and James Cowling
https://pmg.csail.mit.edu/papers/vr-revisited.pdf
-}

type VROp = () -- ?
type VR a = SMM VRState (VRMessage VROp) VRResponse a

tODO :: a
tODO = error "Not implemented yet"

isPrimary :: VR Bool
isPrimary = do
  ViewNumber cVn <- use currentViewNumber
  nodes <- use (configuration.to length)
  rn <- use replicaNumber
  return (rn == cVn `mod` nodes)

checkIsPrimary :: VR ()
checkIsPrimary = guardM isPrimary

checkIsBackup :: ViewNumber -> VR ()
checkIsBackup msgVN = do
  guardM (not <$> isPrimary)
  {-
Replicas only process normal protocol mes-
sages containing a view-number that matches the view-
number they know. If the sender is behind, the receiver
drops the message. If the sender is ahead, the replica
performs a state transfer§
  -}
  -- TODO start state transfer if msgVN > currentViewNumber
  guardM ((== msgVN) <$> use currentViewNumber)

broadCastReplicas :: VRMessage VROp -> VR ()
broadCastReplicas msg = do
  nodes <- use configuration
  ViewNumber cVn <- use currentViewNumber
  forM_ (zip [0..] nodes) $ \ (i, node) -> do
    unless (i == cVn `mod` length nodes) $ send node msg

sendPrimary :: VRMessage VROp -> VR ()
sendPrimary msg = do
  nodes <- use configuration
  ViewNumber cVn <- use currentViewNumber
  send (nodes !! (cVn `mod` length nodes)) msg

addPrepareOk :: OpNumber -> NodeId -> VR Int
addPrepareOk n i = do
  s <- primaryPrepareOk.at n <%= Just . maybe (Set.singleton i) (Set.insert i)
  return (maybe 0 Set.size s)

isQuorum :: Int -> VR Bool
isQuorum x = do
  n <- use (configuration.to length)
  -- `f` is the largest number s.t 2f+1<=n
  let f = (n - 1) `div` 2
  return (f <= x)

{- -- When we get ticks --
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

machine :: Input (VRRequest VROp) (VRMessage VROp) -> VR ()
machine (ClientRequest time c (VRRequest op s)) = do
  {-
1. The client sends a 〈REQUEST op, c, s〉 message to
the primary, where op is the operation (with its ar-
guments) the client wants to run, c is the client-id,
and s is the request-number assigned to the request.
  -}
  checkIsPrimary
  {-
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
        Completed s' r vn
          | s' == s -> do
              -- should check that it has been executed?
              respond c (VRReply vn s r)
              ereturn
          | otherwise -> return ()
        InFlight{} -> return ()
  {-
3. The primary advances op-number, adds the request
to the end of the log, and updates the information
for this client in the client-table to contain the new
request number, s. Then it sends a 〈PREPARE v, m,
n, k〉 message to the other replicas, where v is the
current view-number, m is the message it received
from the client, n is the op-number it assigned to
the request, and k is the commit-number.
  -}
  opNumber += 1
  cOp <- use opNumber
  theLog %= (|> cOp)
  clientTable.at c .= Just (InFlight s)
  v <- use currentViewNumber
  k <- use commitNumber
  broadCastReplicas $ Prepare v op cOp k
machine (InternalMessage time from iMsg) = case iMsg of
  Prepare v m n k -> do
    checkIsBackup v
    {-
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
    -- also look at step 7.
    myK <- use opNumber
    -- i <- use replicaNumber
    when (succ myK /= n) $ do
      -- We haven't processed earlier messages
      -- should start state transfer.
      -- TODO: for now we just drop
      ereturn
    opNumber += 1
    theLog %= (|> n)
    -- TODO: what should be added here? we don't know c or s?
    -- should probably be in m shomehow?
    -- clientTable.at c .= Just (InFlight s)
    sendPrimary $ PrepareOk v n {- i -} -- we don't need to add i since
                                        -- event-loop will add it automatically
  PrepareOk v n -> do
    let i = from
    checkIsPrimary
    {-
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
    howMany <- addPrepareOk n from
    isQ <- isQuorum howMany
    if isQ
      then do
        -- should execute machine
        result <- tODO
        commitNumber .= let OpNumber x = n in CommitNumber x
        clientId <- tODO -- we currently don't have information to go from op-number to clientId
        requestNumber <- tODO
        respond clientId (VRReply v requestNumber result)
      else ereturn
  Commit v k -> do
    checkIsBackup v
    {-
7. When a backup learns of a commit, it waits un-
til it has the request in its log (which may require
state transfer) and until it has executed all earlier
operations. Then it executes the operation by per-
forming the up-call to the service code, increments
its commit-number, updates the client’s entry in the
client-table, but does not send the reply to the client.
    -}
    tODO

sm :: [NodeId] -> NodeId -> SM VRState (VRRequest VROp) (VRMessage VROp) VRResponse
sm otherNodes me = SM (initState otherNodes me) (\i s g -> runSMM (machine i) s g) noTimeouts
