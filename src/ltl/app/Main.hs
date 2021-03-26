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

import GitHash (tGetGitInfo)

import Ltl
import Ltl.Json
import qualified Ltl.Storage as Storage
import Ltl.Prop.Parser (parse)

data Config
  = Check
    { testId :: Int <?> "Which TestId to test",
      runId :: Int <?> "Which RunId to test",
      formula :: Text <?> "LTL Formula to check"
    }
  | Version
  deriving (Generic)

instance ParseRecord Config

gitVersion :: String
gitVersion = $tGetGitInfo

main :: IO ()
main = do
  (cfg, help) <- getWithHelp "LTL checker"
  case cfg of
    Version -> putStrLn gitVersion
    Check{..} -> do
      trace <- Storage.sqliteLoad (unHelpful testId) (unHelpful runId)
      testFormula <- case parse (unHelpful formula) of
        Left s -> error s
        Right x -> pure x
      let r = Result $ check testFormula trace
      TextIO.putStrLn $ AesonText.encodeToLazyText r


data Result = Result
  { result :: Bool
  } deriving (Generic)

instance Aeson.FromJSON Result
instance Aeson.ToJSON Result
