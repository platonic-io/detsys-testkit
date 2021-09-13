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
    { fieldLabelModifier = map (\c -> if c == '_' then '-' else c) }

instance FromJSON Meta where

data SchedulerState = SchedulerState
  { heap :: Heap (Entry UTCTime SchedulerEvent)
  , time :: UTCTime
  , seed :: Seed
  }

initState :: UTCTime -> Seed -> SchedulerState
initState t s = SchedulerState
  { heap = Heap.empty
  , time = t
  , seed = s
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
fakeScheduler executorRef (ClientRequest' "Start" [] cid) = Actor $ do
  r <- Heap.uncons . heap <$> get
  case r of
    Just (Entry time e, heap') -> do
      modify $ \s -> s { heap = heap'
                       , time = time
                       }
      p <- send executorRef (InternalMessage (prettyEvent e))
      on p (\(InternalMessageR (InternalMessage "Ack")) -> undefined)
      clientResponse cid (InternalMessage (show e))
      return (InternalMessage "ok")
    Nothing -> do
      clientResponse cid (InternalMessage "empty heap")
      return (InternalMessage "Done")
  where

-- {"args":{"request":{},"id":"1"},"meta":{"test-id":7,"run-id":1,"logical-time":7},"sent-logical-time":6,"event":"read","from":"frontend","kind":"message","at":"1970-01-01T00:00:10.002343669Z","to":"register2"}

-- {"args":{},"meta":{"test-id":7,"run-id":1,"logical-time":6},"event":"read","from":"client:0","kind":"invoke","at":"1970-01-01T00:00:10Z","to":"frontend"}

    -- XXX: parametrise transport by codec instead?
    prettyEvent :: SchedulerEvent -> String
    prettyEvent = LBS.unpack . encode
fakeScheduler executorRef msg@(InternalMessage "Ack") = Actor $ do
  undefined
  -- does executor send back anything else?
  -- schedule the responses from the executor back on the agenda

  -- cmds <- parseCommands (payload msg)
  -- if no cmds and agenda is empty then stop (how do we contact client? need to save cid?)
  -- else
  -- now <- gets "time"
  -- seed <- gets "seed"
  -- arrivalTime <- genArrivalTime now seed
  -- op2 push arrivalTime (parseCommand resp) %= "heap"
  -- where
  --   parseCommand :: Message -> Datatype
  --   parseCommand (InternalMessage m) = Pair (Text (Text.pack (show m))) (List []) -- XXX: args
fakeScheduler _ msg = error (show msg)

executorCodec :: Codec
executorCodec = Codec encode decode
  where
    encode :: Envelope -> Encode
    encode e = Encode (address (envelopeReceiver e))
                      (LBS.pack (getMessage (envelopeMessage e)))

    decode :: ByteString -> Either String Envelope
    decode bs = case eitherDecode bs of
      Right (ExecutorResponse evs) -> Right $
        Envelope
          { envelopeKind          = ResponseKind
          , envelopeSender        = RemoteRef "executor" 0
          , envelopeMessage       = InternalMessage' "Events" []
          , envelopeReceiver      = RemoteRef "scheduler" 0
          , envelopeCorrelationId = CorrelationId (-1)
          }
      Left err -> error err

data ExecutorResponse = ExecutorResponse
  { events :: [UnscheduledEvent] }
  deriving (Generic, Show)

instance FromJSON ExecutorResponse

data UnscheduledEvent = UnScheduledEvent
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
        [] -> error "parseJSON: impossible" }

t :: Either String ExecutorResponse
t = eitherDecode response

response :: ByteString
response = "{\"events\":[{\"to\":[\"register1\"],\"from\":\"frontend\",\"kind\":\"message\",\"event\":\"write\",\"args\":{\"id\":\"0\",\"request\":{\"value\":1}}},{\"to\":[\"register2\"],\"from\":\"frontend\",\"kind\":\"message\",\"event\":\"write\",\"args\":{\"id\":\"0\",\"request\":{\"value\":1}}}]}"
