module ATMC.Lec5.ViewstampReplication.Machine where

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

checkIsBackup :: VR ()
checkIsBackup = guardM (not <$> isPrimary)

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
    Nothing -> do
      clientTable.at c .= Just (InFlight s)
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
  tODO
machine (InternalMessage time from iMsg) = case iMsg of
  Prepare v m n k -> do
    checkIsBackup
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
    tODO
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
    tODO
  Commit v k -> do
    checkIsBackup
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
sm otherNodes me = SM (initState otherNodes me) (runSMM (Seed 0) . machine)
