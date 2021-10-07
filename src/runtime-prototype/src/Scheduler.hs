{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Scheduler where

import Data.Aeson
import qualified Data.Text.Encoding as Text
import qualified Data.ByteString.Lazy.Char8 as LBS
import Data.Time (UTCTime)
import Database.SQLite.Simple
import GHC.Generics (Generic)

import Scheduler.Event
import Scheduler.Executor
import Scheduler.State
import Scheduler.Agenda (Agenda)
import qualified Scheduler.Agenda as Agenda
import StuntDouble

------------------------------------------------------------------------

type Dropped = Bool
data SchedulerAction
  -- = Tick Time
  = Execute (Time, SchedulerEvent) Dropped
  | Done
  | TimeoutClient (Time, SchedulerEvent) Time -- XXX: what's the second time?

-- XXX: This will need some handling of faults
whatToDo :: {- RunInfo ref -> -} SchedulerState -> SchedulerAction
whatToDo s0 = go s0
  where
    -- XXX: this comes from RunInfo
    clientTimeout = 20
    clientDelay = 20

    go :: SchedulerState -> SchedulerAction
    go s =
      case Agenda.pop (agenda s) of
        Nothing -> Done
        Just (ev@(t, event), agenda') ->
          case lookupClient (from event) s of
            Nothing ->
              -- XXX: check if faults apply here
              Execute ev False
            Just t' ->
              let
                now :: Time
                now = time s
              in
              if now `afterTime` (t' `addTime` clientTimeout)
              then TimeoutClient ev now
              else
                -- Update time. XXX: explain why?
                go (s { agenda = Agenda.push (t `addTime` clientDelay, event) agenda' })

-- echo "{\"tag\":\"InternalMessage'\",\"contents\":[\"CreateTest\",[{\"tag\":\"SInt\",\"contents\":0}]]}" | http POST :3005 && echo "{\"tag\":\"InternalMessage'\",\"contents\":[\"Start\",[]]}" | http POST :3005

fakeScheduler :: RemoteRef -> Message -> Actor SchedulerState
fakeScheduler executorRef (ClientRequest' "CreateTest" [SInt tid] cid) = Actor $ do
  p <- asyncIO (IOQuery "SELECT agenda FROM test_info WHERE test_id = :tid" [":tid" := tid])
  q <- asyncIO (IOQuery "SELECT IFNULL(MAX(run_id), -1) + 1 FROM run_info WHERE test_id = :tid"
                [":tid" := tid])
  on p (\(IOResultR (IORows rs)) -> case parseRows rs of
           Nothing          -> clientResponse cid (InternalMessage "parse error")
           Just [AgendaList es] -> do
             modify $ \s ->
               s { agenda = Agenda.fromList (map (\e -> (at e, e)) es)
                 , testId = Just tid
                 }
             clientResponse cid (InternalMessage (show es)))
  -- XXX: combine `on (p and q)` somehow? the current way we can respond to the
  -- client without having set the runId... Also this current way we can't
  -- really handle an error for the run id select?
  on q (\(IOResultR (IORows [[FInt rid]])) ->
           modify $ \s -> s { runId = Just rid })
  return (InternalMessage "ok")
fakeScheduler executorRef (ClientRequest' "Start" [] cid) =
  let
    step = do
      r <- Agenda.pop . agenda <$> get
      case r of
        Just ((time, e), agenda') -> do
          modify $ \s -> s { agenda = agenda'
                           , time   = time
                           , steps  = succ (steps s)
                           }
          p <- send executorRef (InternalMessage (prettyEvent e))
          on p (\(InternalMessageR (InternalMessage' "Events" args)) -> do
                  -- XXX: we should generate an arrival time here using the seed.
                  -- XXX: with some probability duplicate the event?
                  let Just evs = sequence (map (fromSDatatype time) args)
                      evs' = filter (\e -> kind e /= "ok") (concat evs)
                      agenda' = Agenda.fromList (map (\e -> (at e, e)) evs')
                  modify $ \s -> s { agenda = agenda s `Agenda.union` agenda' }
                  step
               )
        Nothing -> do
          -- The format looks at follows:
          -- LogSend _from (InternalMessage "{\"event\":\"write\",\"args\":{\"value\":1},\"at\":\"1970-01-01T00:00:00Z\",\"kind\":\"invoke\",\"to\":\"frontend\",\"from\":\"client:0\",\"meta\":null}") _to
          -- For network_trace we need:
          -- CREATE VIEW network_trace AS
          --  SELECT
          --    ...
          --    json_extract(data, '$.sent-logical-time')   AS sent_logical_time,
          --    json_extract(data, '$.recv-logical-time')   AS recv_logical_time,
          --    json_extract(data, '$.recv-simulated-time') AS recv_simulated_time,
          --    json_extract(data, '$.dropped')             AS dropped
          --
          -- `sent-logical-time`, can be saved in `LogSend`:
          --
          --    sent-logical-time (or (-> body :sent-logical-time)
          --                          (and is-from-client?
          --                               (:logical-clock data)))]
          -- `recv-logical-time` is `logical-clock` of the next entry
          -- `recv-simulated-time` is `clock` of the next entry

          -- The NetworkTrace event also contains the following fields needed
          -- for jepsen_history:

          --     json_extract(data, '$.jepsen-type')    AS kind,
          --     json_extract(data, '$.jepsen-process') AS process
          --  , ntJepsenType :: Maybe String
          --  , ntJepsenProcess :: Maybe Int


          l <- dumpLog
          s <- get

          -- XXX: something like this needs to be done for each log entry:
          -- p <- asyncIO (IOExecute "INSERT INTO event_log (event, meta, data) \
          --                         \ VALUES (:event, :meta, :data)"
          --                [ ":event" := ("NetworkTrace" :: String)
          --                , ":meta"  := encode (object
          --                                       [ "component" .= ("scheduler" :: String)
          --                                       , "test-id"   .= maybe (error "test id not set") id
          --                                                          (testId s)
          --                                       , "run-id"    .= maybe (error "run id not set") id
          --                                                          (runId s)
          --                                       ])
          --                , ":data"  := encode (object []) -- XXX:
          --                ])

          clientResponse cid (InternalMessage ("{\"steps\":" ++ show (steps s) ++
                                               ",\"test_id\":" ++ show (testId s) ++
                                               ",\"run_id\":" ++ show (runId s) ++
                                               ",\"event_log\":" ++ show l ++
                                               "}"))
  in
    Actor $ do
      step
      return (InternalMessage "ok")
  where
    prettyEvent :: SchedulerEvent -> String
    prettyEvent = LBS.unpack . encode
fakeScheduler _ msg = error (show msg)

-- XXX: Avoid going to string, not sure if we should use bytestring or text though?
entryToData :: Int -> Int -> UTCTime -> Bool -> Timestamped LogEntry -> String
entryToData slt rlt rst d (Timestamped (LogSend _from _to (InternalMessage msg)) _logicalTimestamp _t)
  = addField "sent-logical-time" (show slt) -- XXX: we cannot use _logicalTimestamp
                                            -- here, because its when the event
                                            -- loop sent the message to the
                                            -- executor rather than what we
                                            -- want: when the actor sent the
                                            -- message to the other actor.
  . addField "recv-logical-time" (show rlt)
  . addField "recv-simulated-time" (show (encode rst))
  . addField "dropped" (if d then "true" else "false")
  . replaceEventMessage
  $ msg
  where
    replaceEventMessage ('{' : '"' : 'e' : 'v' : 'e' : 'n' : 't' : msg') = "{\"message" ++ msg'
    addField f v ('{' : msg') = "{\"" ++ f ++ "\":" ++ v ++ "," ++ msg'

data AgendaList = AgendaList [SchedulerEvent]

instance ParseRow AgendaList where
  -- XXX: Text -> ByteString -> JSON, seems unnecessary? We only need the `at`
  -- field for the heap priority, the rest could remain as a text and sent as
  -- such to the executor?
  parseRow [FText t] = case eitherDecodeStrict (Text.encodeUtf8 t) of
    Right es -> Just (AgendaList es)
    Left err -> error (show err)
  parseRow x         = error (show x)
