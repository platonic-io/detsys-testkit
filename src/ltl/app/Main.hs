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
import qualified Data.Text as Text
import qualified Data.Text.IO as TextIO
import qualified Data.Text.Lazy.IO as LazyTextIO
import Ltl
import Ltl.Json
import qualified Ltl.Proof as Proof
import Ltl.Prop (Formula)
import Ltl.Prop.Parser (parse)
import qualified Ltl.Storage as Storage
import Ltl.Traces (Trace)
import Options.Generic

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
      { testId :: Int <?> "Which TestId to test",
        runId :: Int <?> "Which RunId to test",
        formula :: Text <?> "LTL Formula to check"
      }
  | CheckMany
      { testId :: Int <?> "Which TestId to test",
        runId :: Int <?> "Which RunId to test"
      }
  | Version
  deriving (Generic)

instance ParseRecord Config

data Result = Result
  { result :: Bool,
    reason :: String
  }
  deriving (Generic)

instance Aeson.FromJSON Result

instance Aeson.ToJSON Result

data ResultMany = ResultMany
  { results :: [Result]
  }
  deriving (Generic)

instance Aeson.FromJSON ResultMany

instance Aeson.ToJSON ResultMany

checkFormula :: Trace -> Formula -> Result
checkFormula trace formula = Result result (Proof.reason dec)
  where
    dec = check formula trace
    result =
      case dec of
        Proof.No {} -> False
        Proof.Yes {} -> True

getFormulas :: IO [Text]
getFormulas = do
  input <- TextIO.getContents
  pure $ filter (not . Text.null) $ Text.lines input

main :: IO ()
main = do
  (cfg, help) <- getWithHelp "LTL checker"
  case cfg of
    Version -> putStrLn gitHash
    CheckMany {..} -> do
      trace <- Storage.sqliteLoad (unHelpful testId) (unHelpful runId)
      formulas <- getFormulas
      let testFormulas = flip map formulas $ \formula ->
            case parse formula of
              Left s -> error s
              Right x -> x
          results = map (checkFormula trace) testFormulas
      LazyTextIO.putStrLn $ AesonText.encodeToLazyText (ResultMany results)
    Check {..} -> do
      trace <- Storage.sqliteLoad (unHelpful testId) (unHelpful runId)
      testFormula <- case parse (unHelpful formula) of
        Left s -> error s
        Right x -> pure x
      let r = checkFormula trace testFormula
      LazyTextIO.putStrLn $ AesonText.encodeToLazyText r
