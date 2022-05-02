module Main where

import System.Environment
import Control.Exception (assert)

import ATMC.Lec05.EventLoop
import ATMC.Lec05.StateMachine
import ATMC.Lec05.Configuration
import ATMC.Lec05.Codec
import ATMC.Lec05.Debug
import ATMC.Lec05.Random
import ATMC.Lec05.History

import ATMC.Lec02ConcurrentSMTesting

import ATMC.Lec05.ViewstampReplication.State (ReplicatedStateMachine(..))
import qualified ATMC.Lec05.ViewstampReplication.Machine as VR
import ATMC.Lec05.ViewstampReplication.Message

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
        smI :: ReplicatedStateMachine [String] String ()
        smI = ReplicatedStateMachine $ \ s o -> ((), o:s)
        vrSM me = VR.sm (filter (/= me) nodes) me [] smI
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
      let step :: () -> VRRequest () -> ((), VRResponse ())
          step = undefined
          initModel = undefined
      assert (linearisable step initModel (interleavings (blackboxHistory history)))
             (return ())
    _otherwise       -> eventLoopProduction [SomeCodecSM idCodec echoSM]
