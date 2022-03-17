{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralisedNewtypeDeriving#-}
module Dumblog.Journal.StateMachine where

import Data.Binary (Binary)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Lazy.Char8 as LBS8
import Data.TreeDiff (ToExpr)
import Data.Sequence
import GHC.Generics (Generic)

import Dumblog.Journal.Logger
import Dumblog.Journal.Types

------------------------------------------------------------------------

-- This is the main state of Dumblog, which is the result of applying all
-- commands in the log.
data InMemoryDumblog = InMemoryDumblog
  { theLog :: Seq ByteString -- not very memory efficient, but not the point
  , nextIx :: Int
  } deriving stock Generic

instance Binary InMemoryDumblog
instance ToExpr InMemoryDumblog

initState :: InMemoryDumblog
initState = InMemoryDumblog empty 0

runCommand :: Bool -> Logger -> InMemoryDumblog -> Command -> IO (InMemoryDumblog, Response)
runCommand hasBug logger state@(InMemoryDumblog appLog ix) cmd = case cmd of
  Write bs -> do
    logger "Performing a write"
    pure (InMemoryDumblog (appLog |> bs) (ix+1), LBS8.pack (show ix))
  Read i
    | hasBug && ix == 3 -> do
        logger "Weird reset happend"
        pure (InMemoryDumblog empty 0, LBS8.pack "Dumblog!")
    | i < ix -> pure (state, LBS.fromStrict (index appLog i))
    | otherwise -> do
        logger $ "Oh no, request not in log"
        logger $ ("Max index is " ++ show (ix - 1))
        pure (state, "Transaction not in the store!")
    -- ^ XXX: we probably should really signal failure
