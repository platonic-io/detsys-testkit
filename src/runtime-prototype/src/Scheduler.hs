{-# LANGUAGE OverloadedStrings #-}

module Scheduler where

import Data.Text (Text)
import qualified Data.Text as Text
import Control.Exception
import Control.Concurrent.Async

import Database.SQLite.Simple -- XXX: re-export `:=`?

import StuntDouble

------------------------------------------------------------------------

initState :: State
initState = stateFromList
  [ ("heap", heapFromList [(Integer 1, Text "cmd1")])
  , ("time", epoch)
  , ("seed", Integer 0)
  ]

fakeScheduler :: RemoteRef -> Message -> Actor
fakeScheduler executorRef (ClientRequest' "CreateTest" [SInt tid] cid) = Actor $ do
  -- load from db. XXX: need to extend IO module to be able to return Datatype?
  p <- asyncIO (IOQuery "SELECT agenda FROM test_info WHERE test_id = :tid" [":tid" := tid])
  on p (\(IOResultR (IORows entries)) -> undefined)
  undefined
fakeScheduler executorRef (ClientRequest "Start" cid) = Actor $ do
  -- pop agenda end send to executorRef
  r <- pop <$> gets "heap"
  case r of
    Some (Pair {- XXX: time -} cmd heap') -> do
      -- XXX: update "time" time
      update "heap" heap'
      p <- send executorRef (InternalMessage (prettyCommand cmd))
      on p (\(InternalMessageR (InternalMessage "Ack")) -> undefined)
      undefined
    None -> return (InternalMessage "Done") -- XXX: reply to client id?!
    _otherwise -> error "scheduler: start: impossible"
  where
    prettyCommand :: Datatype -> String
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
