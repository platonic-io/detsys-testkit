{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveGeneric #-}

-- This module implements logical time via Lamport clocks, we don't need vector
-- clocks because we can't have events happening concurrently anyway.

module StuntDouble.LogicalTime where

import GHC.Generics (Generic)
import Data.Aeson (FromJSON, ToJSON)
import Data.IORef
import Data.String (IsString)

------------------------------------------------------------------------

newtype NodeName = NodeName String
  deriving (Eq, Ord, IsString, Show, Read, Generic)

instance ToJSON NodeName
instance FromJSON NodeName

data LogicalClock = LogicalClock NodeName (IORef Int)

data LogicalTime = LogicalTime NodeName Int
  deriving (Show, Eq, Read, Generic)

instance ToJSON LogicalTime
instance FromJSON LogicalTime

data Relation = HappenedBeforeOrConcurrently | HappenedAfter

relation :: LogicalTime -> LogicalTime -> Relation
relation (LogicalTime n t) (LogicalTime n' t')
  | t < t' || (t == t' && n < n') = HappenedBeforeOrConcurrently
  | otherwise                     = HappenedAfter

newLogicalClock :: NodeName -> IO LogicalClock
newLogicalClock n = do
  c <- newIORef 0
  return (LogicalClock n c)

-- Upon sending or logging local events we should increment the counter before
-- creating the timestamp.
timestamp :: LogicalClock -> IO LogicalTime
timestamp (LogicalClock n c) = do
  t <- atomicModifyIORef' c (\t -> (t + 1, t + 1))
  return (LogicalTime n t)

-- Upon receving a timestamped message we should update our clock.
update :: LogicalClock -> LogicalTime -> IO LogicalTime
update (LogicalClock n c) (LogicalTime _n' t') =
  atomicModifyIORef' c (\t -> let t'' = max t t' + 1 in (t'', LogicalTime n t''))
