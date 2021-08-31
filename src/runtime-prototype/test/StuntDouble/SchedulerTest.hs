{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module StuntDouble.SchedulerTest where

import Data.Text (Text)
import qualified Data.Text as Text
import Control.Exception
import Control.Concurrent.Async
import Test.HUnit

import StuntDouble

------------------------------------------------------------------------

fakeScheduler :: RemoteRef -> Message -> Actor
fakeScheduler executorRef (ClientRequest "CreateTest" cid) = Actor $ do
  -- load from db
  undefined
fakeScheduler executorRef (ClientRequest "Start" cid) = Actor $ do
  -- pop agenda end send to executorRef
  r <- pop <$> gets "heap"
  case r of
    Some (Pair cmd heap') -> do
      update "heap" heap'
      _ <- send executorRef (InternalMessage (prettyCommand cmd))
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


{-
unit_scheduler :: Assertion
unit_scheduler = do
  aExecutor <- async fakeExecutor
  elog <- emptyEventLog
  let ev = EventLoopName "http://localhost:3003"
  el <- makeEventLoop (Http 3003) ev elog

  let executorRef = RemoteRef ("http://localhost:" ++ show executorPort) 0
      initState = stateFromList [ ("heap", heapFromList [(Integer 1, Text "cmd1")])
                                , ("time", epoch)
                                , ("seed", Integer 0)
                                ]
  catch (do lref <- spawn el (fakeScheduler executorRef) initState
            a <- send el (localToRemoteRef ev lref) (InternalMessage "step")
            reply <- wait a
            reply @?= Just (InternalMessage "stepped"))
    -- (\(e :: SomeException) -> dump el >> eventLog el >>= mapM_ print >> print e)
    (\(e :: SomeException) -> putStrLn "failed")

  quit el
  cancel aExecutor

-}

unit_scheduler :: Assertion
unit_scheduler = return ()
