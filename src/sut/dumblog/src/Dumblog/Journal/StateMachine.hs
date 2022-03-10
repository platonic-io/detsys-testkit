{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralisedNewtypeDeriving#-}
module Dumblog.Journal.StateMachine where

import Data.Aeson (ToJSON(..))
import qualified Data.Aeson as Aeson
import Data.Binary (Binary)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Lazy.Char8 as LBS8
import Data.Text.Encoding (decodeUtf8)
import Data.Sequence
import GHC.Generics (Generic)

import Dumblog.Journal.Types

newtype DumblogByteString = DumblogByteString {innerByteString :: ByteString }
  deriving newtype (Binary)
  deriving stock Generic

instance ToJSON DumblogByteString where
  toJSON (DumblogByteString bs) = Aeson.String (decodeUtf8 bs)


-- This is the main state of Dumblog, which is the result of applying all commands in the log
data InMemoryDumblog = InMemoryDumblog
  { theLog :: Seq DumblogByteString -- not very memory efficient, but not the point
  , nextIx :: Int
  } deriving stock Generic

instance Binary InMemoryDumblog where
instance ToJSON InMemoryDumblog

initState :: InMemoryDumblog
initState = InMemoryDumblog empty 0

-- this could be pure?
runCommand :: InMemoryDumblog -> Command -> IO (InMemoryDumblog, Response)
runCommand state@(InMemoryDumblog appLog ix) cmd = case cmd of
  Write bs -> pure (InMemoryDumblog (appLog |> DumblogByteString bs) (ix+1), LBS8.pack (show ix))
  Read i
    | i < ix -> pure (state, LBS.fromStrict $ innerByteString (index appLog i))
    | otherwise -> pure (state, "Transaction not in the store!") -- we probably should really signal failure
