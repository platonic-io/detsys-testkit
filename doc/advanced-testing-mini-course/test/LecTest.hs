module LecTest where

import qualified Lec04FaultInjection as Lec04
import Test.Tasty.HUnit

------------------------------------------------------------------------

unit_faultTest :: IO ()
unit_faultTest = Lec04.unit_faultTest
