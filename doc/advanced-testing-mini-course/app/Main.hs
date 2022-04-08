module Main where

import ATMC.Lec5SimulationTestingV3
import ATMC.Lec5.StateMachine
import ATMC.Lec5.Codec

------------------------------------------------------------------------

main :: IO ()
main = eventLoopProduction [SomeCodecSM idCodec echoSM]
