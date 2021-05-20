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

executorPort :: Int
executorPort = 3004

fakeExecutor :: IO ()
fakeExecutor = do
  t <- httpTransport executorPort
  e <- transportReceive t
  envelopeMessage e @?= envelopeMessage e -- XXX: check if cmd is of the right shape
  let resp = replyEnvelope e (Message "XXX: needs the right shape")
  transportSend t resp

fakeScheduler :: RemoteRef -> Message -> Actor
fakeScheduler executor (Message "step") = do
  Just (cmd, heap') <- "heap" ^. pop
  "heap" .= (heap' :: Datatype)
  a <- remoteCall executor (Message (prettyCommand cmd))
  Left (Just resp) <- unsafeAwait (Left a)
  -- assert resp -- XXX: check if of the right shape
  now <- get "time"
  seed <- get "seed"
  arrivalTime <- genArrivalTime now seed
  op2 push arrivalTime (parseCommand resp) %= "heap"
  return (Now (Message "stepped"))

  where
    parseCommand :: Message -> Datatype
    parseCommand (Message m) = Pair (Text (Text.pack (show m))) (List []) -- XXX: args

    prettyCommand :: Text -> String
    prettyCommand _ = "XXX: command"

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
            a <- send el (localToRemoteRef ev lref) (Message "step")
            reply <- wait a
            reply @?= Just (Message "stepped"))
    (\(e :: SomeException) -> dump el >> eventLog el >>= mapM_ print >> print e)

  quit el
  cancel aExecutor
