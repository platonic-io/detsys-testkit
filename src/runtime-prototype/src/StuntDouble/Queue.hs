module StuntDouble.Queue where

import Control.Monad
import Data.IORef
import qualified Data.Vector.Mutable as Vec

------------------------------------------------------------------------

data Queue a = Queue
  { capacity :: !Int
  , size     :: !(IORef Int)
  , rear     :: !(IORef Int)
  , queue    :: !(Vec.IOVector a)
  }

newQueue :: Int -> IO (Queue a)
newQueue cap = Queue <$> pure cap <*> newIORef 0 <*> newIORef 0 <*> Vec.new cap

clear :: Queue a -> IO ()
clear q = do
  writeIORef (size q) 0
  writeIORef (rear q) 0
  Vec.clear (queue q)

isEmpty :: Queue a -> IO Bool
isEmpty q = do
  r  <- readIORef (rear q)
  sz <- readIORef (size q)
  return (sz - 1 - r <= 0)

enqueue :: a -> Queue a -> IO Bool
enqueue x q = do
  sz <- readIORef (size q)
  if sz == capacity q
  then return False
  else do
    r <- readIORef (rear q)
    Vec.unsafeWrite (queue q) ((r + sz) `mod` capacity q) x
    modifyIORef' (size q) succ
    return True

dequeue :: Queue a -> IO (Maybe a)
dequeue q = do
  empty <- isEmpty q
  if empty
  then return Nothing
  else do
    r <- readIORef (rear q)
    x <- Vec.unsafeRead (queue q) r
    modifyIORef' (rear q) (\r -> r + 1 `mod` capacity q)
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
  ssz <- readIORef (size source)
  tsz <- readIORef (size target)
  if ssz + tsz > capacity target
  then return False
  else do
    sr <- readIORef (rear source)
    flip mapM_ [sr..ssz - 1] $ \ix -> do -- XXX: abstract out looping
      x <- Vec.unsafeRead (queue source) ix
      tr <- readIORef (rear target) -- XXX: avoid rereading each iteration...
      tsz <- readIORef (size target) -- XXX: also
      Vec.unsafeWrite (queue target) ((tr + tsz) `mod` capacity target) x
      modifyIORef' (size target) succ
    clear source
    return True

display :: Show a => Queue a -> IO ()
display q = do
  putStr "Queue"
  putStr " "
  putStr (show (capacity q))
  putStr " "
  sz <- readIORef (size q)
  putStr (show sz)
  putStr " "
  r <- readIORef (rear q)
  putStr (show r)
  putStr " "
  putStr "["
  r <- readIORef (rear q)
  sz <- readIORef (size q)
  flip mapM_ [r..sz - 1] $ \ix -> do
    x <- Vec.unsafeRead (queue q) ix
    putStr (show x)
    unless (ix == sz - 1) $ do
      putStr ", "
  putStrLn "]"

-- XXX: create proper test suite...
qtest :: IO ()
qtest = do
 q <- newQueue 2 :: IO (Queue Int)
 -- enqueue 1 q
 -- enqueue 2 q
 -- Just x <- dequeue q
 -- print x
 -- display q
 -- Just y <- dequeue q
 -- print y
 -- display q

 enqueue 1 q
 enqueue 2 q
 display q
 q2 <- newQueue 4 :: IO (Queue Int)
 enqueue 3 q2
 enqueue 4 q2
 b <- move q2 q
 print b
 display q2
