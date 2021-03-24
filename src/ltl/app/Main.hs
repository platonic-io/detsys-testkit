{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Text as AesonText
import qualified Data.Text.Lazy.IO as TextIO
import Options.Generic

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

main :: IO ()
main = do
  (cfg, help) <- getWithHelp "LTL checker"
  case cfg of
    Version -> putStrLn "<GIT VERSION NOT IMPLEMENTED>"
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
