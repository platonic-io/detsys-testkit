{-# LANGUAGE OverloadedStrings #-}

module Ldfi.Solver where

import Data.Text (Text)
import qualified Data.Text as T
import Data.Map (Map)
import qualified Data.Map as Map

import Ldfi.Prop

------------------------------------------------------------------------
-- * Solver

data Solution = NoSolution | Solution (Map String Bool)
  deriving Show

marshal :: Solution -> Text
marshal NoSolution            = "{\"faults\": []}"
marshal (Solution assignment) =
  "{\"faults\":" `mappend` marshalList vars `mappend `"}"
  where
    vars = [ var | (var, true) <- Map.toList assignment, true ]

marshalList :: [String] -> Text
marshalList = T.pack . show

data Solver m = Solver
  { solve :: Formula -> m Solution }
