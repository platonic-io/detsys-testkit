{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
module ATMC.Lec5.SmartBFT.Machine where

import ATMC.Lec5.StateMachine
import ATMC.Lec5.StateMachineDSL
import ATMC.Lec5.Time (Time)

import ATMC.Lec5.SmartBFT.Messages
import ATMC.Lec5.SmartBFT.State

type Response = ()
type BFT a = SMM SBFTState SBFTMessage Response a

tODO = error "Not implemented yet"

broadCast :: SBFTMessage -> BFT ()
broadCast = tODO

proposalHash :: Proposal -> Hash
proposalHash _ = "hash"

-- check conditions, if all are true run the continuation
-- the conditions shouldn't mutate state (or send messages)
preConditions :: [BFT Bool] -> BFT () -> BFT ()
preConditions []     k = k
preConditions (c:cs) k = do
  b <- c
  if b
    then preConditions cs k
    else return ()

appendTransactions :: Time -> [TransactionContent] -> BFT ()
appendTransactions now txs = do
  tODO -- append to pendingQueue

transactionsToPropose :: BFT [TransactionContent]
transactionsToPropose = do
  tODO -- fetch from pendingQueue

maybeSendPropose :: Time -> RoundId -> BFT ()
maybeSendPropose now cId = preConditions
  [ do -- node must be a leader
      me_ <- use (globalState.me)
      leader <- use (globalState.leader)
      return (me_ == leader)
  , do -- height must be >= lastInitiatedRound
      h <- use (globalState.height)
      last <- use (consensusState.lastInitiatedRound)
      return (h >= last)
  , do -- cannot send new proposal if state transfer is in progress
      not <$> use (globalState.stateTransferInProgress)
  , do -- cannot send new proposal if leader election is in progress and consenses paused
      not <$> use (consensusState.consensusPaused)
  ] $ do
  me <- use (globalState.me)
  txs <- transactionsToPropose
  rId <- use (globalState.regency)
  let proposal = Proposal txs now
      propose = ConsensusMessage cId rId me (Propose (proposalHash proposal) proposal)
  consensusState.lastInitiatedRound .= cId
  handlePropose now cId rId proposal
  broadCast propose

handleClientAppend :: Time -> TransactionContent -> BFT ()
handleClientAppend now tx = do
  let txs = [tx]
  me_ <- use (globalState.me)
  appendTransactions now txs
  h <- use (globalState.height)
  maybeSendPropose now (succ h)
  broadCast (Append me_ txs)
  -- we should register the client

handleAppend :: Time -> [TransactionContent] -> BFT ()
handleAppend now txs = do
  appendTransactions now txs
  h <- use (globalState.height)
  maybeSendPropose now (succ h)

updateRound :: RoundId -> (Round -> BFT Round) -> BFT ()
updateRound = tODO

updateVote :: Round -> RegencyId -> (ConsensusVotes -> BFT ConsensusVotes) -> BFT Round
updateVote = tODO

handlePropose :: Time -> RoundId -> RegencyId -> Proposal -> BFT ()
handlePropose now cId rId proposal = updateRound cId $ \ round -> do
  -- check if proposal already exists
  -- check request comes from leader
  updateVote round rId $ \ votes -> do
    return (votes { cvPropose = Just proposal})

-- TODO we should check that the sender is in the network
machine :: Input SBFTRequest SBFTMessage -> BFT ()
machine (ClientRequest now cid req) = case req of
  ClientAppend tx -> handleClientAppend now tx
  Get tx -> tODO
machine (InternalMessage now nid msg) = case msg of
  Append from txs -> handleAppend now txs
  ConsensusMessage cId rId from cmsg -> case cmsg of
    Propose _ prop -> handlePropose now cId rId prop
    _ -> tODO
  _ -> tODO

initState :: SBFTState
initState = tODO

sm :: SM SBFTState SBFTRequest SBFTMessage Response
sm = SM initState (runSMM (Seed 0) . machine) noTimeouts
