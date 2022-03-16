{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralisedNewtypeDeriving#-}
module Dumblog.Journal.StateMachine where

import Data.Binary (Binary)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Lazy.Char8 as LBS8
import Data.Text.Encoding (decodeUtf8)
import Data.TreeDiff (ToExpr)
import Data.Sequence
import GHC.Generics (Generic)

import Dumblog.Journal.Logger
import Dumblog.Journal.Types
import Journal.Internal.Metrics (incrCounter)

------------------------------------------------------------------------

-- This is the main state of Dumblog, which is the result of applying all
-- commands in the log.
data InMemoryDumblog = InMemoryDumblog
  { theLog :: Seq ByteString -- not very memory efficient, but not the point
  , nextIx :: Int
  , hasBug :: Bool
  } deriving stock Generic

instance Binary InMemoryDumblog
instance ToExpr InMemoryDumblog

initState :: InMemoryDumblog
initState = InMemoryDumblog empty 0 False

runCommand :: Logger -> InMemoryDumblog -> Command -> IO (InMemoryDumblog, Response)
runCommand logger state@(InMemoryDumblog appLog ix hasBug) cmd = case cmd of
  Write bs -> do
    logger "Performing a write"
    pure (InMemoryDumblog (appLog |> {- DumblogByteString-} bs) (ix+1) hasBug, LBS8.pack (show ix))
  Read i
    | hasBug && ix == 3 -> do
        logger "Weird reset happend"
        pure (InMemoryDumblog empty 0 hasBug, LBS8.pack "Dumblog!")
    | i < ix -> pure (state, LBS.fromStrict $ {-innerByteString-} (index appLog i))
    | otherwise -> do
        logger $ "Oh no, request not in log"
        logger $ ("Max index is " ++ show (ix - 1))
        pure (state, "Transaction not in the store!")
    -- ^ XXX: we probably should really signal failure
