{-# LANGUAGE TemplateHaskell #-}

module Ldfi.GitHash where

import Ldfi.GitHashImpl

------------------------------------------------------------------------

version :: String
version = $tGetGitInfo
