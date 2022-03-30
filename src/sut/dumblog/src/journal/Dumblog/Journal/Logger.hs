module Dumblog.Journal.Logger where

import Data.Foldable
import Data.IORef
import Data.Sequence

type Logger = LogItem -> IO ()

nullLogger :: Logger
nullLogger = const (pure ())

ioLogger :: Logger
ioLogger = putStrLn

type LogItem = String
newtype QueueLogger = QueueLogger (IORef (Seq LogItem))

newQueueLogger :: IO QueueLogger
newQueueLogger = QueueLogger <$> newIORef empty

queueLogger :: QueueLogger -> Logger
queueLogger (QueueLogger ref) = \logItem -> atomicModifyIORef' ref $ \xs -> (xs |> logItem, ())

flushQueue :: QueueLogger -> IO [LogItem]
flushQueue (QueueLogger ref) = atomicModifyIORef' ref $ \xs -> (empty, toList xs)
