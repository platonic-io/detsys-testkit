{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Storage where

import Control.Exception (throwIO)
import Database.SQLite.Simple
import System.Environment (getEnv)
import System.FilePath ((</>))
import System.IO.Error (catchIOError, isDoesNotExistError)

getDbPath :: IO String
getDbPath = do
  getEnv "DETSYS_DB"
    `catchIOError` \(e :: catchIOError) ->
      if isDoesNotExistError e
        then do
          home <- getEnv "HOME"
          return (home </> ".detsys.db")
        else throwIO e

sqliteGetRuns :: Int -> IO [Int]
sqliteGetRuns testId = do
  path <- getDbPath
  conn <- open path
  fmap (map fromOnly) $
    queryNamed
      conn
      "SELECT run_id \
      \FROM run_info \
      \WHERE test_id = :testId"
      [":testId" := testId]
