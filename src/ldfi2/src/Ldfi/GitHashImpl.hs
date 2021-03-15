{-# LANGUAGE TemplateHaskell #-}

module Ldfi.GitHashImpl where

import Control.Exception
import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import System.Environment
import System.IO.Error

------------------------------------------------------------------------

-- We need this indirection, because you can't define and splice in the
-- same module...

getVersionFromEnv :: IO String
getVersionFromEnv = do
  getEnv "DETSYS_LDFI_VERSION"
    `catchIOError` \e ->
      if isDoesNotExistError e
        then return "unknown"
        else throwIO e

tGetGitInfo :: Q Exp
tGetGitInfo = do
  s <- runIO getVersionFromEnv
  lift s
