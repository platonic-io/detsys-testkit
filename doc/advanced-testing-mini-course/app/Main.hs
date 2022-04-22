{-# LANGUAGE TypeApplications #-}

module Main where

import System.Environment
import Control.Exception (assert)

import ATMC.Lec5.EventLoop
import ATMC.Lec5.StateMachine
import ATMC.Lec5.Configuration
import ATMC.Lec5.Codec
import ATMC.Lec5.Debug
import ATMC.Lec5.Random
import ATMC.Lec5.History

import ATMC.Lec2ConcurrentSMTesting

import ATMC.Lec5.ViewstampReplication.State (ReplicatedStateMachine(..))
import qualified ATMC.Lec5.ViewstampReplication.Machine as VR
import ATMC.Lec5.ViewstampReplication.Message

------------------------------------------------------------------------

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["--simulation"] -> do
      h <- newHistory
      eventLoopSimulation (Seed 0) echoAgenda h [SomeCodecSM idCodec echoSM]
      _history <- readHistory h
      putStrLn "Can't print history yet, need Show/Pretty constraints for parameters..."
    ["vr", "--simulation", fp] -> do
      h <- newHistory
      let
        nodes = map NodeId [0..4]
        smI = ReplicatedStateMachine $ \ s o -> (o, s)
        vrSM me = VR.sm (filter (/= me) nodes) me () smI
        printItem label prefix thing =
          putStrLn $ "\x1b[31m" <> label <> ":\x1b[0m " <> prefix <> show thing
        printE (HistEvent n bs inp as msgs) = do
          putStrLn "\n\x1b[32mNew Entry\x1b[0m"
          printItem "Node" " " n
          printItem "State before" "\n" bs
          printItem "Input" " " inp
          printItem "State after" "\n" as
          printItem "Sent messages" "" ""
          mapM_ (\x -> putStrLn $ "  " <> show x) msgs
      eventLoopSimulation (Seed 0) VR.agenda h [ SomeCodecSM VR.vrCodec (vrSM me)
                                               | me <- nodes]
      history <- readHistory h
      mapM_ printE history
      writeDebugFile fp history
      let bbHistory = blackboxHistory history
          step :: () -> VRRequest () -> ((), VRResponse ())
          step = undefined
          initModel = undefined
      assert (linearisable step initModel (interleavings bbHistory)) (return ())
    _otherwise       -> eventLoopProduction [SomeCodecSM idCodec echoSM]
