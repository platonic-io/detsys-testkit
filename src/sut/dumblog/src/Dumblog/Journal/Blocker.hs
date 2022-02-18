module Dumblog.Journal.Blocker
  (Blocker, emptyBlocker, blockUntil, wakeUp)
where

import Control.Concurrent (threadDelay)

import Control.Concurrent.MVar (MVar, newEmptyMVar, takeMVar, putMVar)
import Data.IORef

import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import Data.Tuple (swap)

newtype Blocker x = Blocker (IORef (IntMap (MVar x)))

emptyBlocker :: IO (Blocker x)
emptyBlocker = Blocker <$> newIORef IntMap.empty

blockUntil :: Blocker x -> Int -> IO x
blockUntil (Blocker b) key = do
  mv <- newEmptyMVar
  atomicModifyIORef' b $ \m -> (IntMap.insert key mv m, ())
  takeMVar mv

wakeUp :: Blocker x -> Int -> x -> IO Bool
wakeUp (Blocker b) key response = do
  mmv <- go 10
  case mmv of
    Nothing -> pure False
    Just mv -> do
      putMVar mv response
      pure True
  where
    go 0 = pure Nothing
    go n = do
      m <- atomicModifyIORef' b $ swap . IntMap.updateLookupWithKey (const $ const Nothing) key
      case m of
        Nothing -> do
          threadDelay 10
          go (n-1)
        Just x -> pure m
