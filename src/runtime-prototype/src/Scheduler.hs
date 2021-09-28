{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Scheduler where

import Control.Concurrent.Async
import Control.Exception
import Data.Aeson
import Data.Char (toLower)
import Data.ByteString.Lazy.Char8 (ByteString)
import qualified Data.ByteString.Lazy.Char8 as LBS
import Data.Heap (Entry(Entry), Heap)
import qualified Data.Heap as Heap
import Data.Maybe (fromJust)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import Data.Time (UTCTime)
import Database.SQLite.Simple
import GHC.Generics (Generic)

import StuntDouble

------------------------------------------------------------------------

data SchedulerEvent = SchedulerEvent
  { kind  :: String
  , event :: String
  , args  :: Data.Aeson.Value
  , to    :: String
  , from  :: String
  , at    :: UTCTime
  , meta  :: Maybe Meta
  }
  deriving (Generic, Eq, Ord, Show)

instance FromJSON SchedulerEvent
instance ToJSON SchedulerEvent

data Meta = Meta
  { test_id      :: Int
  , run_id       :: Int
  , logical_time :: Int
  }
  deriving (Generic, Eq, Ord, Show)

instance ToJSON Meta where
  toEncoding = genericToEncoding defaultOptions
    -- The executor expects kebab-case.
    { fieldLabelModifier = map (\c -> if c == '_' then '-' else c) }

instance FromJSON Meta where

data SchedulerState = SchedulerState
  { heap  :: Heap (Entry UTCTime SchedulerEvent)
  , time  :: UTCTime
  , seed  :: Seed
  , steps :: Int
  }

initState :: UTCTime -> Seed -> SchedulerState
initState t s = SchedulerState
  { heap  = Heap.empty
  , time  = t
  , seed  = s
  , steps = 0
  }

data Agenda = Agenda [SchedulerEvent]

instance ParseRow Agenda where
  -- XXX: Text -> ByteString -> JSON, seems unnecessary? We only need the `at`
  -- field for the heap priority, the rest could remain as a text and sent as
  -- such to the executor?
  parseRow [FText t] = case eitherDecodeStrict (Text.encodeUtf8 t) of
    Right es -> Just (Agenda es)
    Left err -> error (show err)
  parseRow x         = error (show x)

-- echo "{\"tag\":\"InternalMessage'\",\"contents\":[\"CreateTest\",[{\"tag\":\"SInt\",\"contents\":0}]]}" | http POST :3005

fakeScheduler :: RemoteRef -> Message -> Actor SchedulerState
fakeScheduler executorRef (ClientRequest' "CreateTest" [SInt tid] cid) = Actor $ do
  p <- asyncIO (IOQuery "SELECT agenda FROM test_info WHERE test_id = :tid" [":tid" := tid])
  on p (\(IOResultR (IORows rs)) -> case parseRows rs of
           Nothing          -> clientResponse cid (InternalMessage "parse error")
           Just [Agenda es] -> do
             modify $ \s ->
               s { heap = Heap.fromList (map (\e -> Entry (at e) e) es) }
             clientResponse cid (InternalMessage (show es)))
  return (InternalMessage "ok")
fakeScheduler executorRef (ClientRequest' "Start" [] cid) =
  let
    step = do
      r <- Heap.uncons . heap <$> get
      case r of
        Just (Entry time e, heap') -> do
          modify $ \s -> s { heap  = heap'
                           , time  = time
                           , steps = succ (steps s)
                           }
          p <- send executorRef (InternalMessage (prettyEvent e))
          on p (\(InternalMessageR (InternalMessage' "Events" args)) -> do
                  -- XXX: we should generate an arrival time here using the seed.
                  -- XXX: with some probability duplicate the event?
                  let Just evs = sequence (map (fromSDatatype time) args)
                      evs' = filter (\e -> kind e /= "ok") (concat evs)
                      heap' = Heap.fromList (map (\e -> Entry (at e) e) evs')
                  modify $ \s -> s { heap = heap s `Heap.union` heap' }
                  step
               )
        Nothing -> do
          -- XXX: Add `getLog :: Actor Log` and then process the log here?
          -- The format looks at follows:
          -- LogSend _from (InternalMessage "{\"event\":\"write\",\"args\":{\"value\":1},\"at\":\"1970-01-01T00:00:00Z\",\"kind\":\"invoke\",\"to\":\"frontend\",\"from\":\"client:0\",\"meta\":null}") _to
          -- For network_trace we need:
          -- CREATE VIEW network_trace AS
          --  SELECT
          --    json_extract(meta, '$.test-id')             AS test_id,
          --    json_extract(meta, '$.run-id')              AS run_id,
          --    json_extract(data, '$.message')             AS message,
          --    json_extract(data, '$.args')                AS args,
          --    json_extract(data, '$.from')                AS sender,
          --    json_extract(data, '$.to')                  AS receiver,
          --    json_extract(data, '$.kind')                AS kind,
          --    json_extract(data, '$.sent-logical-time')   AS sent_logical_time,
          --    json_extract(data, '$.recv-logical-time')   AS recv_logical_time,
          --    json_extract(data, '$.recv-simulated-time') AS recv_simulated_time,
          --    json_extract(data, '$.dropped')             AS dropped
          --
          -- Scheduler gets the test_id upon `CreateTest` and it can figure out
          -- which is the next `run_id` from the db using:
          --
          --  SELECT IFNULL(MAX(run_id), -1) + 1 as `run-id` FROM run_info WHERE test_id = :tid
          --
          -- `message` is `"event"`
          -- `args`, `from`, `to`, `kind` is the same
          -- `sent-logical-time`, can be saved in `LogSend`:
          --
          --    sent-logical-time (or (-> body :sent-logical-time)
          --                          (and is-from-client?
          --                               (:logical-clock data)))]
          -- `recv-logical-time` is `logical-clock` of the next entry
          -- `recv-simulated-time` is `clock` of the next entry

          s <- get
          clientResponse cid (InternalMessage ("{\"steps\":" ++ show (steps s) ++ "}"))
  in
    Actor $ do
      step
      return (InternalMessage "ok")
  where
    prettyEvent :: SchedulerEvent -> String
    prettyEvent = LBS.unpack . encode
fakeScheduler _ msg = error (show msg)

executorCodec :: Codec
executorCodec = Codec encode decode
  where
    encode :: Envelope -> Encode
    encode e = Encode (address (envelopeReceiver e))
                      (getCorrelationId (envelopeCorrelationId e))
                      (LBS.pack (getMessage (envelopeMessage e)))

    decode :: ByteString -> Either String Envelope
    decode bs = case eitherDecode bs of
      Right (ExecutorResponse evs corrId) -> Right $
        Envelope
          { envelopeKind          = ResponseKind
          , envelopeSender        = RemoteRef "executor" 0
          -- XXX: going to sdatatype here seems suboptimal...
          , envelopeMessage       = InternalMessage' "Events" (map toSDatatype evs)
          , envelopeReceiver      = RemoteRef "scheduler" 0
          , envelopeCorrelationId = corrId
          }
      Left err -> error err

data ExecutorResponse = ExecutorResponse
  { events :: [UnscheduledEvent]
  , corrId :: CorrelationId
  }
  deriving (Generic, Show)

instance FromJSON ExecutorResponse

data UnscheduledEvent = UnscheduledEvent
  { ueKind  :: String
  , ueEvent :: String
  , ueArgs  :: Data.Aeson.Value
  , ueTo    :: [String]
  , ueFrom  :: String
  }
  deriving (Generic, Eq, Ord, Show)

instance FromJSON UnscheduledEvent where
  parseJSON = genericParseJSON defaultOptions
    { fieldLabelModifier = \s -> case drop (length ("ue" :: String)) s of
        (x : xs) -> toLower x : xs
        [] -> error "parseJSON: impossible, unless the field names of `UnscheduledEvent` changed" }

toSDatatype :: UnscheduledEvent -> SDatatype
toSDatatype (UnscheduledEvent kind event args to from) =
  SList [SString kind, SString event, SValue args, SList (map SString to), SString from]

fromSDatatype :: UTCTime -> SDatatype -> Maybe [SchedulerEvent]
fromSDatatype at (SList
  [SString kind, SString event, SValue args, SList tos, SString from])
  = Just [ SchedulerEvent kind event args to from at Nothing | SString to <- tos ]
fromSDatatype _at _d = Nothing
