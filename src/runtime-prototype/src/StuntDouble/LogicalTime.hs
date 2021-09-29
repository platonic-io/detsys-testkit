{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- This module implements logical time via Lamport clocks, we don't need vector
-- clocks because we can't have events happening concurrently anyway.

module StuntDouble.LogicalTime where

import Data.IORef
import Data.String (IsString)

------------------------------------------------------------------------

newtype NodeName = NodeName String
  deriving (Eq, Ord, IsString, Show)

data LogicalTime = LogicalTime NodeName (IORef Int)

data LogicalTimestamp = LogicalTimestamp NodeName Int
  deriving Show

data Relation = HappenedBeforeOrConcurrently | HappenedAfter

relation :: LogicalTimestamp -> LogicalTimestamp -> Relation
relation (LogicalTimestamp n t) (LogicalTimestamp n' t')
  | t < t' || (t == t' && n < n') = HappenedBeforeOrConcurrently
  | otherwise                     = HappenedAfter

newLogicalTime :: NodeName -> IO LogicalTime
newLogicalTime n = do
  c <- newIORef 0
  return (LogicalTime n c)

-- Upon sending or logging local events we should increment the counter before
-- creating the timestamp.
timestamp :: LogicalTime -> IO LogicalTimestamp
timestamp (LogicalTime n c) = do
  t <- atomicModifyIORef' c (\t -> (t + 1, t + 1))
  return (LogicalTimestamp n t)

-- Upon receving a timestamped message we should update our clock.
update :: LogicalTime -> LogicalTimestamp -> IO ()
update (LogicalTime _n c) (LogicalTimestamp _n' t') =
  atomicModifyIORef' c (\t -> (max t t' + 1, ()))
