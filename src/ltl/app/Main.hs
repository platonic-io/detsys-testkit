{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}
module Main where

import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Text as AesonText
import qualified Data.Text.Lazy.IO as TextIO
import Options.Generic

import Ltl
import Ltl.Json
import Ltl.Prop.Parser (parse)
import qualified Ltl.Storage as Storage

------------------------------------------------------------------------

-- When building with Bazel we generate a module containing the git commit hash
-- at compile-time.
#ifdef __BAZEL_BUILD__
import GitHash
-- When building with cabal we expect the git commit hash to be passed in via
-- CPP flags, i.e. `--ghc-option=-D__GIT_HASH__=\"X\"`.
#elif defined __GIT_HASH__
gitHash :: String
gitHash = __GIT_HASH__
#else
gitHash :: String
gitHash = "unknown"
#endif

------------------------------------------------------------------------

data Config
  = Check
    { testId  :: Int <?> "Which TestId to test",
      runId   :: Int <?> "Which RunId to test",
      formula :: Text <?> "LTL Formula to check"
    }
  | Version
  deriving (Generic)

instance ParseRecord Config

main :: IO ()
main = do
  (cfg, help) <- getWithHelp "LTL checker"
  case cfg of
    Version -> putStrLn gitHash
    Check{..} -> do
      trace <- Storage.sqliteLoad (unHelpful testId) (unHelpful runId)
      testFormula <- case parse (unHelpful formula) of
        Left s  -> error s
        Right x -> pure x
      let r = Result $ check testFormula trace
      TextIO.putStrLn $ AesonText.encodeToLazyText r


data Result = Result
  { result :: Bool
  } deriving (Generic)

instance Aeson.FromJSON Result
instance Aeson.ToJSON Result
