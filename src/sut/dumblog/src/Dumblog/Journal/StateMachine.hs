{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
module Dumblog.Journal.StateMachine where

import Data.Binary (Binary)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Lazy.Char8 as LBS8
import GHC.Generics (Generic)

import Dumblog.Journal.Types

-- This is the main state of Dumblog, which is the result of applying all commands in the log
data InMemoryDumblog = InMemoryDumblog
  { theLog :: [ByteString] -- not very memory efficient, but not the point
  , nextIx :: Int
  } deriving Generic

instance Binary InMemoryDumblog where

initState :: InMemoryDumblog
initState = InMemoryDumblog [] 0

-- this could be pure?
runCommand :: InMemoryDumblog -> Command -> IO (InMemoryDumblog, Response)
runCommand state@(InMemoryDumblog log ix) cmd = case cmd of
  Write bs -> pure (InMemoryDumblog (bs:log) (ix+1), "Appended to position: " <> LBS8.pack (show ix))
  Read i
    | i < ix -> pure (state, LBS.fromStrict $ log !! (ix - 1 - i))
    | otherwise -> pure (state, "Transaction not in the store!") -- we probably should really signal failure
