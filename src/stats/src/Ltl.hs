{-# LANGUAGE DeriveGeneric #-}

module Ltl where

import qualified Data.Aeson as Aeson
import Data.String (fromString)
import GHC.Generics (Generic)
import System.Process (proc, readCreateProcess)

data LtlResult = LtlResult
  { result :: Bool
  }
  deriving (Generic)

instance Aeson.FromJSON LtlResult

data LtlResults = LtlResults
  { results :: [LtlResult]
  }
  deriving (Generic)

instance Aeson.FromJSON LtlResults

ltl :: Int -> Int -> [String] -> IO [Bool]
ltl testId runId formulas = do
  let p =
        proc
          "detsys-ltl"
          [ "checkmany",
            "--testId",
            show testId,
            "--runId",
            show runId
          ]
  res <- readCreateProcess p (unlines formulas)
  case Aeson.decode (fromString res) of
    Nothing -> error "Can't decode result from LTL"
    Just r -> pure (map result $ results r)
