module Scheduler where

import Data.Char (toLower)
import Data.List (sort)
import qualified Data.Either as Either
import Data.Map (Map)
import qualified Data.Map as Map

import qualified Scheduler.Agenda as Agenda
import Scheduler.Agenda (AgendaEntry)
import Scheduler.Event
import qualified Scheduler.Client as Client
import qualified Scheduler.Executor as Executor
import qualified Scheduler.Random as Random
import qualified Scheduler.Time as Time
import qualified Scheduler.Trace as Trace
import Scheduler.Types

type Fault = ()

type CompenentId = String
type Components = [CompenentId]

  {-
data SchedulerCommand
  = LoadTest TestId -- seems unecessary
  | CreateRun TestId Seed [Fault]
  | RegisterExecutor ExecutorId Components -- also weird
  | Reset
  | Start
-}

type ExecutorId = String -- URL
type Reactor = String -- just the name

-- we only have one url so maybe always send to it?
type Topology ref = Map Reactor ref

data RunInfo ref = RunInfo
  { totalExecutors :: Int
  , faults :: [Fault]
  , topology :: Topology ref
  -- , tickFrequency :: Double
  , minTimeNs :: Time.Timestamp
  , maxTimeNs :: Time.Timestamp
  , clientTimeout :: Time.Duration
  , clientDelay :: Time.Duration
  }

type Dropped = Bool
data Action
  -- = Tick Time.Timestamp
  = Execute AgendaEntry Dropped
  | Done
  | TimeoutClient AgendaEntry Time.Timestamp

-- This will need some handling of faults
whatToDo :: Monad m =>
  Agenda.Capability m ->
  Client.Capability m ->
  Time.Capability m ->
  RunInfo ref -> m Action
whatToDo agendaC clientC timeC ri = go
  where
    go = do
      mEntry <- Agenda.pop agendaC
      case mEntry of
        Nothing -> return Done
        Just ent -> do
          let sender = from $ Agenda.theEvent ent
          b <- Client.isClientActive clientC sender
          case b of
            Nothing ->
              -- TODO check if faults apply here
              return $ Execute ent False
            Just t -> do
              cur <- Time.currentSimulatedClock timeC
              if cur `Time.after` (t `Time.timeAdd` clientTimeout ri)
                then return $ TimeoutClient ent cur
                else do
                  let ent' = Agenda.updateTime ent (clientDelay ri) -- update time
                  Agenda.addEntries agendaC [ent']
                  go

data SR = Sender | Receiver

traceEvent :: Dropped -> Bool -> Time.LogicalTime -> Maybe (SR, Int) -> AgendaEntry
  -> Trace.NetworkTrace
traceEvent d timeout now mClientId ae = Trace.NetworkTrace
    { Trace.ntMessage = event aevent,
      Trace.ntArgs = args aevent,
      Trace.ntFrom = from aevent,
      Trace.ntTo = to aevent,
      Trace.ntKind = kind aevent,
      Trace.ntSentLogicalTime = case sentAt aevent of
        Nothing -> Time.theLogicalTime now - 1 -- ??
        Just x -> x,
      Trace.ntRecvLogicalTime = Time.theLogicalTime now,
      Trace.ntRecvSimulatedTime = Agenda.theTime ae,
      Trace.ntDropped = d,
      Trace.ntJepsenType =fmap (clientType . fst) mClientId,
      Trace.ntJepsenProcess = fmap snd mClientId
    }
  where
    aevent = Agenda.theEvent ae
    clientType _ | timeout = "info"
    clientType Sender = "invoke"
    clientType Receiver = "ok"

clientP :: Monad m => Client.Capability m -> String -> String -> m (Maybe (SR, Int))
clientP clientC fromA toA = do
  mp <- Client.isClient clientC fromA
  case mp of
    Nothing -> do
      mp' <- Client.isClient clientC toA
      case mp' of
        Nothing -> return Nothing
        Just p -> return (Just (Receiver, p))
    Just p -> return (Just (Sender, p))

emitEvent :: Monad m =>
  Trace.Capability m ->
  Client.Capability m ->
  TestId -> RunId -> Dropped -> Time.LogicalTime -> AgendaEntry -> m ()
emitEvent traceC clientC tid rid d now ae = do
  mclient <- clientP clientC (from $ Agenda.theEvent ae) (to $ Agenda.theEvent ae)
  Trace.emitEvent traceC tid rid (traceEvent d False now mclient ae)

emitTimeout :: Monad m =>
  Trace.Capability m ->
  Client.Capability m ->
  TestId -> RunId -> Dropped -> Time.LogicalTime -> AgendaEntry -> m ()
emitTimeout traceC clientC tid rid d now ae = do
  mclient <- clientP clientC (from $ Agenda.theEvent ae) (to $ Agenda.theEvent ae)
  Trace.emitEvent traceC tid rid (traceEvent d True now mclient ae)

partitionOutEvent :: Monad m => Client.Capability m -> Time.LogicalTime ->
  Executor.Events -> m ([ClientResponse], [(Event, Maybe Time.Duration)])
partitionOutEvent clientC now = fmap Either.partitionEithers . mapM f . sort . concatMap translate
  where
    -- f :: (Event, Maybe Time.Duration) -> m (Either ClientResponse Event)
    f (e, d) = do
      mc <- Client.isClient clientC (to e)
      case mc of
        Nothing -> return $ Right (e, d)
        Just _p -> do
          -- should we deActivateClient here?
          Client.deActivateClient clientC (to e)
          return $ Left $ ClientResponse e
    kindTy Executor.Ok = "ok"
    kindTy Executor.Message = "message"
    kindTimerTy Executor.Timer = "timer"
    translate :: Executor.OutEvent -> [(Event, Maybe Time.Duration)]
    translate (Executor.OEUnscheduledEvent use) = do
      toA <- Executor.ueTo use
      return (Event
              { kind = kindTy (Executor.ueKind use),
                event = Executor.ueEvent use,
                args = Executor.ueArgs use,
                to = toA,
                from = Executor.ueFrom use,
                sentAt = Just (Time.theLogicalTime now)
              }, Nothing)
    translate (Executor.OETimer te) =
      return (Event
              { kind = kindTimerTy (Executor.teKind te),
                event = "timer",
                args = Executor.teArgs te,
                to = Executor.teFrom te, -- note we use from
                from = Executor.teFrom te,
                sentAt = Just (Time.theLogicalTime now)
              }, Just $ Executor.teDuration te)


resolveClientResponses :: Monad m => TestId -> RunId -> RunInfo ref
  -> Time.Capability m
  -> [ClientResponse] -> m ()
resolveClientResponses _testId _runId _runInfo _timeC [] = pure ()
resolveClientResponses _testId _runId _runInfo timeC _cr = do
  now <- Time.currentSimulatedClock timeC
  -- we set the time to the same, but we bump the logical time
  Time.advanceTime timeC now Time.BumpLogical
  -- TODO
  -- we should emit events here
  pure ()

scheduleEvents :: Monad m
  => Random.Capability m
  -> Agenda.Capability m
  -> Time.Capability m
  -> [(Event, Maybe Time.Duration)] -> m ()
scheduleEvents randomC agendaC timeC aes = do
  deltas <- Random.gen randomC (length aes)
  clock <- Time.currentSimulatedClock timeC
  let entries = zipWith (f clock) aes deltas
  Agenda.addEntries agendaC entries
  where
    f clock (tevent, mdur) delta =
      let clock' = case mdur of
            Nothing -> clock
            Just dur -> Time.timeAdd clock dur
      in Agenda.mkEntry (Time.timeAdd clock' delta) tevent

-- not sure about this one chef
toInEvent :: TestId -> RunId -> Time.LogicalTime -> AgendaEntry -> Executor.ScheduledEvent
toInEvent testId runId logicalTime entry = Executor.ScheduledEvent
  { Executor.at = Agenda.theTime entry,
    Executor.kind = kindTy,
    Executor.from = from aevent,
    Executor.to = to aevent,
    Executor.event = event aevent,
    Executor.args = args aevent,
    Executor.meta = meta
  }
  where
    aevent = Agenda.theEvent entry
    kindTy = case map toLower (kind aevent) of
      "timer" -> Executor.KETimer
      "ok" -> Executor.KEClient
      "invoke" -> Executor.KEClient -- hmm
      "message" -> Executor.KEInternalMessage
      unknown -> error $ "Unknown kind: " <> unknown
    meta = Executor.MetaInfo
      { Executor.testId = theTestId testId,
        Executor.runId = theRunId runId,
        Executor.logicalTime = Time.theLogicalTime logicalTime
      }

allRefs :: RunInfo ref -> [ref]
allRefs ri = Map.elems $ topology ri

senderRef :: RunInfo ref -> String -> ref
senderRef ri x = topology ri Map.! x

run :: Monad m => TestId -> RunId -> RunInfo ref
  -> Agenda.Capability m
  -> Client.Capability m
  -> Random.Capability m
  -> Trace.Capability m
  -> Executor.Capability ref m
  -> Time.Capability m
  -> m ()
run testId runId runInfo agendaC clientC randomC traceC executorC timeC = go
  where
    go = do
      act <- whatToDo agendaC clientC timeC runInfo
      case act of
        {-
        Tick now -> do
          Time.advanceTime timeC now Time.KeepLogical
          events <- forM (allRefs runInfo) $ \ ref -> Executor.tick executorC ref now
          let (cr, entries) = partitionOutEvent (sort $ concat events)
          resolveClientResponses testId runId runInfo timeC  cr
          scheduleEvents randomC agendaC timeC entries
          go
-}
        Execute ae dropped
          | dropped -> do
              let now = Agenda.theTime ae
              Time.advanceTime timeC now Time.BumpLogical
              lnow <- Time.currentLogicalClock timeC
              emitEvent traceC clientC testId runId dropped lnow ae
              go
          | otherwise -> do
              -- if client request we need to add it to state
              let now = Agenda.theTime ae
              Time.advanceTime timeC now Time.BumpLogical
              currentLogicalTime <- Time.currentLogicalClock timeC
              emitEvent traceC clientC testId runId dropped currentLogicalTime ae
              let ref = senderRef runInfo (to $ Agenda.theEvent ae)
              let ie = toInEvent testId runId currentLogicalTime ae
              events <- case Executor.kind ie of
                Executor.KEInternalMessage -> Executor.execute executorC ref ie
                Executor.KEClient -> Executor.execute executorC ref ie
                Executor.KETimer -> Executor.timer executorC ref ie
              (cr, entries) <- partitionOutEvent clientC currentLogicalTime events
              resolveClientResponses testId runId runInfo timeC cr
              scheduleEvents randomC agendaC timeC entries
              go
        Done -> return ()
        TimeoutClient ae now -> do
          Time.advanceTime timeC now Time.BumpLogical  -- should this really bump logical?
          lnow <- Time.currentLogicalClock timeC
          emitTimeout traceC clientC testId runId False lnow ae
          go
