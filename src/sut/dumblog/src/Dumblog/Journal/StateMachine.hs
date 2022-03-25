{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralisedNewtypeDeriving#-}
module Dumblog.Journal.StateMachine where

import Data.Binary (Binary)
import Data.ByteString.Lazy (ByteString)
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

runCommand :: Bool -> Logger -> InMemoryDumblog -> Input -> IO (InMemoryDumblog, Output)
runCommand hasBug logger state@(InMemoryDumblog appLog ix) input =
  case input of
    ClientRequest req -> fmap (fmap ClientResponse) $ case req of
      Write bs -> do
        logger "Performing a write"
        pure (InMemoryDumblog (appLog |> bs) (ix+1), OK (LBS8.pack (show ix)))
      Read i
        | hasBug && ix == 3 -> do
            logger "Weird reset happend"
            pure (initState, Error (LBS8.pack "Dumblog!"))
        | i < ix -> pure (state, OK (index appLog i))
        | otherwise -> do
            logger $ "Oh no, request not in log"
            logger $ ("Max index is " ++ show (ix - 1))
            pure (state, NotFound)
