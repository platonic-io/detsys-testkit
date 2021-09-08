module StuntDouble.Queue where

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

enqueue :: a -> Queue a -> IO Bool
enqueue x q = do
  n <- size q
  if n == capacity q -- XXX: >=
  then return False
  else do
    j <- rear q
    Vec.unsafeWrite (qQueue q) ((j + n) `mod` capacity q) x
    modifyIORef' (qSize q) succ
    return True

enqueueList :: [a] -> Queue a -> IO Bool
enqueueList xs q = go xs
  where
    go [] = return True
    go (x : xs) = do
      b <- enqueue x q
      if b
      then go xs
      else return False

dequeue :: Queue a -> IO (Maybe a)
dequeue q = do
  empty <- isEmpty q
  if empty
  then return Nothing
  else do
    j <- rear q
    x <- Vec.unsafeRead (qQueue q) j
    modifyIORef' (qSize q) pred
    modifyIORef' (qRear q) (\j -> (j + 1) `mod` capacity q)
    return (Just x)

iter :: (a -> b -> IO b) -> b -> Queue a -> IO b
iter s e q = do
  empty <- isEmpty q
  if empty then return e
  else do
    Just x <- dequeue q
    ih <- iter s e q
    s x ih

move :: Queue a -> Queue a -> IO Bool
move target source = do
  ssz <- size source
  tsz <- size target
  if ssz + tsz > capacity target
  then return False
  else do
    sr <- rear source
    flip mapM_ [sr..ssz - 1] $ \ix -> do -- XXX: abstract out looping
      x <- Vec.unsafeRead (qQueue source) ix
      tr <- rear target -- XXX: avoid rereading each iteration...
      tsz <- size target -- XXX: also
      Vec.unsafeWrite (qQueue target) ((tr + tsz) `mod` capacity target) x
      modifyIORef' (qSize target) succ
    clear source
    return True

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
