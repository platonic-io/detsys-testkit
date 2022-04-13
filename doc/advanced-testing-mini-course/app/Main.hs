module Main where

import System.Environment

import ATMC.Lec5SimulationTestingV3
import ATMC.Lec5.StateMachine
import ATMC.Lec5.Configuration
import ATMC.Lec5.Codec

------------------------------------------------------------------------

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["--simulation"] -> eventLoopSimulation echoAgenda [SomeCodecSM idCodec echoSM]
    _otherwise       -> eventLoopProduction [SomeCodecSM idCodec echoSM]
