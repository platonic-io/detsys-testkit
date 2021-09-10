{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Scheduler where

import Control.Concurrent.Async
import Control.Exception
import Data.Aeson
import GHC.Generics (Generic)
import Data.Heap (Entry(Entry), Heap)
import qualified Data.Heap as Heap
import Data.Text (Text)
import qualified Data.Text.Encoding as Text
import qualified Data.Text as Text
import Data.Time (UTCTime)
import Database.SQLite.Simple

import StuntDouble

------------------------------------------------------------------------

data SchedulerEvent = SchedulerEvent
  { kind  :: String
  , event :: String
  , args  :: Data.Aeson.Value
  , to    :: String
  , from  :: String
  , at    :: UTCTime
  }
  deriving (Generic, Eq, Ord, Show)

instance FromJSON SchedulerEvent where

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
  parseRow [FText t] = case eitherDecodeStrict (Text.encodeUtf8 t) of
    Right es -> Just (Agenda es)
    Left err -> error (show err)
  parseRow x         = error (show x)

fakeScheduler :: RemoteRef -> Message -> Actor SchedulerState
fakeScheduler executorRef (ClientRequest' "CreateTest" [SInt tid] cid) = Actor $ do
  p <- asyncIO (IOQuery "SELECT agenda FROM test_info WHERE test_id = :tid" [":tid" := tid])
  on p (\(IOResultR (IORows rs)) -> case parseRows rs of
           Nothing          -> clientResponse cid (InternalMessage "parse error")
           Just [Agenda es] -> clientResponse cid (InternalMessage (show es)))
  return (InternalMessage "ok")
fakeScheduler executorRef (ClientRequest "Start" cid) = Actor $ do
  -- pop agenda end send to executorRef
  r <- Heap.uncons . heap <$> get
  case r of
    Just (Entry time cmd, heap') -> do
      modify $ \s -> s { heap = heap'
                       , time = time
                       }
      p <- send executorRef (InternalMessage (prettyCommand cmd))
      on p (\(InternalMessageR (InternalMessage "Ack")) -> undefined)
      undefined
    Nothing -> return (InternalMessage "Done") -- XXX: reply to client id?!
  where
    prettyCommand :: SchedulerEvent -> String
    prettyCommand = undefined
fakeScheduler executorRef msg@(InternalMessage "Ack") = Actor $ do
  undefined
  -- does executor send back anything else?
  -- schedule the responses from the executor back on the agenda

  -- XXX: we need to make messages be able to have args/fields/payload
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
