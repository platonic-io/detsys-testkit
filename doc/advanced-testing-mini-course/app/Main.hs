module Main where

import System.Environment

import ATMC.Lec5SimulationTestingV3
import ATMC.Lec5.StateMachine
import ATMC.Lec5.Configuration
import ATMC.Lec5.Codec
import ATMC.Lec5.History

------------------------------------------------------------------------

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["--simulation"] -> do
      h <- newHistory
      eventLoopSimulation echoAgenda h [SomeCodecSM idCodec echoSM]
      _history <- readHistory h
      putStrLn "Can't print history yet, need Show/Pretty constraints for parameters..."
    _otherwise       -> eventLoopProduction [SomeCodecSM idCodec echoSM]
