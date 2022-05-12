module Lec05.ErrorReporter where

import Data.IORef

type ErrorReporter = String -> IO ()

reportError :: ErrorReporter
reportError = putStrLn

data Collector = Collector (IORef [String])

newCollector :: IO Collector
newCollector = Collector <$> newIORef []

reportWithCollector :: Collector -> ErrorReporter
reportWithCollector (Collector ref) str = do
  atomicModifyIORef ref $ \xs -> (str:xs, ())
  putStrLn str

readFromCollector :: Collector -> IO [String]
readFromCollector (Collector ref) = reverse <$> readIORef ref
