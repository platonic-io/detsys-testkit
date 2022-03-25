{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module Dumblog.Journal.StateMachine where

import Data.Binary (Binary)
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy.Char8 as LBS8
import Data.Maybe (isJust)
import Data.Sequence
import Data.TreeDiff (ToExpr)
import GHC.Generics (Generic)

import Dumblog.Journal.Logger
import Dumblog.Journal.Types

------------------------------------------------------------------------

-- This is the main state of Dumblog, which is the result of applying all
-- commands in the log.
data InMemoryDumblog = InMemoryDumblog
  { theLog :: Seq ByteString -- not very memory efficient, but not the point
  , nextIx :: Int
  , peerPort :: Maybe Int
  } deriving stock Generic

instance Binary InMemoryDumblog
instance ToExpr InMemoryDumblog

initState :: InMemoryDumblog
initState = InMemoryDumblog empty 0 Nothing

runCommand :: Bool -> Logger -> InMemoryDumblog -> Input -> IO (InMemoryDumblog, Output)
runCommand hasBug logger state@(InMemoryDumblog appLog ix mPeerPort) input =
  case input of
    ClientRequest req -> case req of
      Read i
        | hasBug && ix == 3 -> do
            logger "Weird reset happend"
            pure (initState, ClientResponse (Error (LBS8.pack "Dumblog!")))
        | i < ix -> pure (state, ClientResponse (OK (index appLog i)))
        | otherwise -> do
            logger $ "Oh no, request not in log"
            logger $ ("Max index is " ++ show (ix - 1))
            pure (state, ClientResponse NotFound)
      Write bs
        | isJust mPeerPort -> do
            logger "Forwarding write to backup"
            pure (InMemoryDumblog (appLog |> bs) (ix+1) mPeerPort,
                  InternalMessageOut (Backup (ix + 1) bs))
        | otherwise -> do
            logger "Performing a write"
            pure (InMemoryDumblog (appLog |> bs) (ix+1) mPeerPort,
                  ClientResponse (OK (LBS8.pack (show (ix + 1)))))

    InternalMessageIn msg -> case msg of
      Backup ix' bs -> do
        logger "Performing a backup"
        pure (InMemoryDumblog (appLog |> bs) ix' mPeerPort, InternalMessageOut (Ack ix'))
      Ack ix' -> do
        logger "Acknowledging a backup"
        pure (state, ClientResponse (OK (LBS8.pack (show ix'))))

    AdminCommand (Connect port) -> do
      logger ("Adding peer on port: " ++ show port)
      pure (state { peerPort = Just port }, AdminResponse)
