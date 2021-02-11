{-# LANGUAGE TemplateHaskell #-}

module Ldfi.GitHash where

import GitHash

------------------------------------------------------------------------

version :: String
version = giHash gi ++ if giDirty gi then " (dirty)" else ""
  where
    gi = $$tGitInfoCwd
