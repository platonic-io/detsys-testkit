{-# LANGUAGE ScopedTypeVariables #-}

module Dumblog.Journal.Blocker
  (Blocker, Key, sequenceNumber, cancel, newKey, emptyBlocker, blockUntil, wakeUp)
where

import Control.Concurrent.MVar (MVar, newEmptyMVar, takeMVar, putMVar)
import Data.IORef
import Journal.Types.AtomicCounter (AtomicCounter)
import qualified Journal.Types.AtomicCounter as AtomicCounter

import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import Data.Tuple (swap)

data Blocker x = Blocker (IORef (IntMap (MVar x))) AtomicCounter

data Key x = Key
  { sequenceNumber :: Int
  , mVar :: MVar x
  }

emptyBlocker :: Int -> IO (Blocker x)
emptyBlocker startingSequence = Blocker
  <$> newIORef IntMap.empty
  <*> AtomicCounter.newCounter startingSequence

newKey :: Blocker x -> IO (Key x)
newKey (Blocker b c) = do
  key <- AtomicCounter.incrCounter 1 c
  mv <- newEmptyMVar
  atomicModifyIORef' b $ \m -> (IntMap.insert key mv m, ())
  pure (Key key mv)

cancel :: Blocker x -> Key x -> IO ()
cancel (Blocker b _) key =
  atomicModifyIORef' b $ \m -> (IntMap.delete (sequenceNumber key) m, ())

blockUntil :: Key x -> IO x
blockUntil key = takeMVar (mVar key)

wakeUp :: forall a. Blocker a -> Int -> a -> IO Bool
wakeUp (Blocker b _) key response = do
  mmv <- atomicModifyIORef' b $ swap . IntMap.updateLookupWithKey (const $ const Nothing) key
  case mmv of
    Nothing -> pure False
    Just mv -> do
      putMVar mv response
      pure True
