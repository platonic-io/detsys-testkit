module ATMC.Lec3.Queue where

import Control.Monad
import Data.IORef
import qualified Data.Vector.Mutable as Vec

------------------------------------------------------------------------

data Queue a = Queue
  { qCapacity :: !Int
  , qSize     :: !(IORef Int)
  , qRear     :: !(IORef Int)
  , qQueue    :: !(Vec.IOVector a)
  }

newQueue :: Int -> IO (Queue a)
newQueue cap = Queue <$> pure cap <*> newIORef 0 <*> newIORef 0 <*> Vec.new cap

capacity :: Queue a -> Int
capacity = qCapacity

size :: Queue a -> IO Int
size = readIORef . qSize

-- XXX: Don't export.
rear :: Queue a -> IO Int
rear = readIORef . qRear

clear :: Queue a -> IO ()
clear q = do
  writeIORef (qSize q) 0
  writeIORef (qRear q) 0
  Vec.clear (qQueue q)

isEmpty :: Queue a -> IO Bool
isEmpty q = do
  sz <- size q
  return (sz == 0)

enqueue :: Queue a -> a -> IO Bool
enqueue q x = do
  n <- size q
  if n >= capacity q
  then return False
  else do
    j <- rear q
    Vec.unsafeWrite (qQueue q) ((j + n) `mod` capacity q) x
    modifyIORef' (qSize q) succ
    return True

dequeue :: Queue a -> IO (Maybe a)
dequeue q = do
  empty <- isEmpty q
  if empty
  then return Nothing
  else do
    j <- rear q
    x <- Vec.unsafeRead (qQueue q) j
    modifyIORef' (qSize q) (\sz -> sz - 1)
    modifyIORef' (qRear q) (\j -> (j + 1) `mod` capacity q)
    return (Just x)

display :: Show a => Queue a -> IO ()
display q = do
  putStrLn "Queue"
  putStr "  { capacity = "
  putStrLn (show (capacity q))
  putStr "  , size = "
  sz <- size q
  putStrLn (show sz)
  putStr "  , rear = "
  r <- rear q
  putStrLn (show r)
  putStr "  , queue = "
  putStr "["
  r <- rear q
  sz <- size q
  flip mapM_ [r..sz - 1] $ \ix -> do
    x <- Vec.unsafeRead (qQueue q) ix
    putStr (show x)
    unless (ix == sz - 1) $ do
      putStr ", "
  putStrLn "]"
