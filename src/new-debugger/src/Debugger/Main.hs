{-# LANGUAGE OverloadedStrings #-}
module Debugger.Main where

import Data.Aeson (decodeFileStrict)
import qualified Data.Vector as Vector
import System.Environment (getArgs)
import System.Exit (die)

import Debugger.UI (AppState, mkAppState, runApp)
import Debugger.State

------------------------------------------------------------------------

debugMain :: IO ()
debugMain = do
  as <- getArgs
  case as of
    [f] -> do
      content <- decodeFileStrict f
      case content of
        Nothing -> do die "Can't parse debug-file"
        Just values -> runApp (mkAppState (fromRepr values))
    _ -> die "Debugger wants exactly one input, which is the debug-file"
