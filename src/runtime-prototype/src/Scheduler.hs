{-# LANGUAGE OverloadedStrings #-}

module Scheduler where

import Control.Concurrent.Async
import Control.Exception
import Data.Heap (Entry(Entry), Heap)
import qualified Data.Heap as Heap
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Time (UTCTime)
import Database.SQLite.Simple -- XXX: re-export `:=`?

import StuntDouble

------------------------------------------------------------------------

data SchedulerCommand = SchedulerCommand

data SchedulerState = SchedulerState
  { heap :: Heap (Entry UTCTime SchedulerCommand)
  , time :: UTCTime
  , seed :: Seed
  }

initState :: UTCTime -> Seed -> SchedulerState
initState t s = SchedulerState
  { heap = Heap.empty
  , time = t
  , seed = s
  }

fakeScheduler :: RemoteRef -> Message -> Actor SchedulerState
fakeScheduler executorRef (ClientRequest' "CreateTest" [SInt tid] cid) = Actor $ do
  -- load from db. XXX: need to extend IO module to be able to return Datatype?
  p <- asyncIO (IOQuery "SELECT agenda FROM test_info WHERE test_id = :tid" [":tid" := tid])
  on p (\(IOResultR (IORows entries)) -> undefined)
  undefined
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
    prettyCommand :: SchedulerCommand -> String
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
