{-# LANGUAGE TemplateHaskell #-}

module Ldfi.GitHashImpl where

import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import System.Environment


------------------------------------------------------------------------
-- We need this indirection, because you can't define and splice in the same module..

tGetGitInfo :: Q (TExp String)
tGetGitInfo = do
    s <- runIO $ getEnv "DETSYS_LDFI_VERSION"
    liftTyped s
