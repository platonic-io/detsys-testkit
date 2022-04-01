module Main where

import Dumblog.Common.Constants
import Dumblog.Metrics.Main

------------------------------------------------------------------------

main :: IO ()
main = metricsMain dUMBLOG_PORT
