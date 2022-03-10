{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
module Dumblog.Journal.StateMachine where

import Data.Binary (Binary)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Lazy.Char8 as LBS8
import Data.Sequence
import GHC.Generics (Generic)

import Dumblog.Journal.Types
import Dumblog.Journal.Metrics
import Journal.Internal.Metrics (incrCounter)

------------------------------------------------------------------------

-- This is the main state of Dumblog, which is the result of applying all
-- commands in the log.
data InMemoryDumblog = InMemoryDumblog
  { theLog :: Seq ByteString -- not very memory efficient, but not the point
  , nextIx :: Int
  } deriving Generic

instance Binary InMemoryDumblog

initState :: InMemoryDumblog
initState = InMemoryDumblog empty 0

runCommand :: InMemoryDumblog -> Command -> IO (InMemoryDumblog, Response)
runCommand state@(InMemoryDumblog appLog ix) cmd = case cmd of
  Write bs -> pure (InMemoryDumblog (appLog |> bs) (ix+1), LBS8.pack (show ix))
  Read i
    | i < ix -> pure (state, LBS.fromStrict $ index appLog i)
    | otherwise -> pure (state, "Transaction not in the store!")
    -- XXX: ^ we probably should really signal failure
