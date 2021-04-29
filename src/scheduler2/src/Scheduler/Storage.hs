{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Scheduler.Storage where

import Database.SQLite.Simple

import System.Environment (getEnv)
import System.FilePath ((</>))
import System.IO.Error (catchIOError, isDoesNotExistError)

import Scheduler.Types

getDbPath :: IO String
getDbPath = do
  getEnv "DETSYS_DB"
    `catchIOError` \(e :: catchIOError) ->
      if isDoesNotExistError e
        then do
          home <- getEnv "HOME"
          return (home </> ".detsys.db")
        else throwIO e
