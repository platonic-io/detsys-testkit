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

data LogicalClock = LogicalClock NodeName (IORef LogicalTimeInt)

newtype LogicalTimeInt = LogicalTimeInt Int
  deriving (Show, Eq, Read, Generic, Num, Ord, Enum)

instance ToJSON LogicalTimeInt
instance FromJSON LogicalTimeInt

data LogicalTime = LogicalTime NodeName LogicalTimeInt
  deriving (Show, Eq, Read, Generic)

instance ToJSON LogicalTime
instance FromJSON LogicalTime

getLogicalTimeInt :: LogicalTime -> LogicalTimeInt
getLogicalTimeInt (LogicalTime _n i) = i

succLogicalTime :: LogicalTime -> LogicalTime
succLogicalTime (LogicalTime n i) = LogicalTime n (succ i)

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
  t' <- atomicModifyIORef' c (\t -> let t' = t + 1 in (t', t'))
  return (LogicalTime n t')

timestampInt :: LogicalClock -> IO LogicalTimeInt
timestampInt (LogicalClock n c) = atomicModifyIORef' c (\t -> let t' = t + 1 in (t', t'))

-- Upon receving a timestamped message we should update our clock.
update :: LogicalClock -> LogicalTimeInt -> IO LogicalTime
update (LogicalClock n c) t' =
  atomicModifyIORef' c (\t -> let t'' = max t t' + 1
                              in (t'', LogicalTime n t''))
