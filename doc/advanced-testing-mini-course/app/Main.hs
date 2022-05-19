module Main where

import System.Environment
import Control.Exception (assert)
import Control.Monad (unless)

import Lec05.ErrorReporter
import Lec05.EventLoop
import Lec05.StateMachine
import Lec05.Configuration
import Lec05.Codec
import Lec05.Debug
import Lec05.Deployment
import Lec05.Network
import Lec05.Random
import Lec05.History
import Lec05.Time

import Lec02ConcurrentSMTesting

import Lec05.ViewstampReplication.State (ReplicatedStateMachine(..))
import qualified Lec05.ViewstampReplication.Machine as VR
import Lec05.ViewstampReplication.Message

------------------------------------------------------------------------

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["--simulation"] -> do
      h <- newHistory
      _collector <- eventLoopSimulation (Seed 0) echoAgenda h [SomeCodecSM idCodec echoSM]
      _history <- readHistory h
      putStrLn "Can't print history yet, need Show/Pretty constraints for parameters..."
    ["vr", "--simulation", fp] -> do
      h <- newHistory
      let
        nodes = map NodeId [0..4]
        smI :: ReplicatedStateMachine [String] String ()
        smI = ReplicatedStateMachine $ \ s o -> ((), o:s)
        delta = 10 -- time for primary to do re-broadcast
        vrSM me = VR.vrSM (filter (/= me) nodes) me delta [] smI
        printItem label prefix thing =
          putStrLn $ "\x1b[33m" <> label <> ":\x1b[0m " <> prefix <> show thing
        printE (HistEvent' d (HistEvent n bs inp as msgs)) = do
          putStrLn "\n\x1b[32mNew Entry\x1b[0m"
          printItem "Node" " " n
          printItem "State before" "\n" bs
          printItem "Input" (case d of { DidDrop -> "\x1b[31m[DROPPED]\x1b[0m "; DidArrive -> " "}) inp
          printItem "State after" "\n" as
          printItem "Sent messages" "" ""
          mapM_ (\x -> putStrLn $ "  " <> show x) msgs
        fs = FailureSpec (NetworkFaults 0.15)
        endTime = addTime 600 epoch
      collector <- eventLoopFaultySimulation (Seed 6) (VR.agenda endTime) h fs
        [ SomeCodecSM VR.vrCodec (vrSM me) | me <- nodes]
      history <- readHistory h
      mapM_ printE history
      -- let's print the errors again so they are easier to see.
      reportedErrors <- readFromCollector collector
      unless (null reportedErrors) (putStrLn "")
      mapM_ putStrLn reportedErrors
      writeDebugFile fp history
      let step :: () -> VRRequest () -> ((), VRResponse ())
          step = undefined
          initModel = undefined
      assert (linearisable step initModel (interleavings (blackboxHistory (fmap heEvent history))))
             (return ())
    _otherwise       -> eventLoopProduction [SomeCodecSM idCodec echoSM]
