{-# LANGUAGE ScopedTypeVariables #-}

module StuntDouble.SchedulerTest where

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
