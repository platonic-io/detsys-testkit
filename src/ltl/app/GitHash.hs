module GitHash where

import Control.Exception (throwIO)
import Language.Haskell.TH.Syntax (Exp, Q, runIO, lift)
import System.Environment (getEnv)
import System.IO.Error (catchIOError, isDoesNotExistError)

getVersionFromEnv :: IO String
getVersionFromEnv = do
  getEnv "DETSYS_LTL_VERSION"
    `catchIOError` \e ->
      if isDoesNotExistError e
        then return "unknown"
        else throwIO e

tGetGitInfo :: Q Exp
tGetGitInfo = do
  s <- runIO getVersionFromEnv
  lift s
