{-# LANGUAGE Arrows #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TypeFamilies #-}

module Lec09.Pipeline where

import Control.Applicative
import Control.Arrow
import Control.Category (Category, id, (.))
import Control.Concurrent
import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Monad
import Data.ByteString.Lazy (ByteString)
import Numeric.Natural
import Prelude hiding (id, (.))
import System.Random

import Lec09.FreeFunc hiding ((>>>))

------------------------------------------------------------------------

data Pipeline m a b where
  ArrM     :: (a -> m b) -> Pipeline m a b
  Lift     :: FreeFunc a b -> Pipeline m a b
  ConsumeP :: Pipeline m a ()
  Sequence :: Pipeline m a b -> Pipeline m b c -> Pipeline m a c
  Fork     :: Pipeline m a (a, a)
  Par      :: Pipeline m a c -> Pipeline m b d -> Pipeline m (a, b) (c, d)
  Split    :: (a -> Bool) -> Pipeline m a (Either a a)
  Or       :: Pipeline m a c -> Pipeline m b d -> Pipeline m (Either a b) (Either c d)
  Combine  :: Pipeline m a b -> Pipeline m a b -> Pipeline m a b

instance Monad m => Category (Pipeline m) where
  id    = ArrM return
  g . f = Sequence f g

instance Monad m => Arrow (Pipeline m) where
  arr   = ArrM . (return .)
  (***) = Par

  (&&&) :: Pipeline m a b -> Pipeline m a c -> Pipeline m a (b, c)
  f &&& g = Fork >>> f `Par` g

instance Monad m => ArrowChoice (Pipeline m) where
  (+++) :: Pipeline m a c -> Pipeline m b d -> Pipeline m (Either a b) (Either c d)
  (+++) = Or

  (|||) :: Pipeline m b d -> Pipeline m c d -> Pipeline m (Either b c) d
  f ||| g = f +++ g >>> arr untag
    where
      untag (Left  x) = x
      untag (Right y) = y

instance (Monad m, MonadPlus m) => ArrowZero (Pipeline m) where
  zeroArrow = ArrM (const mzero)

instance (Monad m, MonadPlus m) => ArrowPlus (Pipeline m) where
  (<+>) = Combine

------------------------------------------------------------------------

runPipeline :: Monad m => Pipeline m a b -> a -> m b
runPipeline (ArrM f)       = f
runPipeline (Sequence f g) = runPipeline f >=> runPipeline g
runPipeline _ = error "TODO"

data Deployment a = Deployment
  { dQueue  :: TBQueue a
  , dAsyncs :: [Async ()]
  }

qUEUE_SIZE :: Natural
qUEUE_SIZE = 65536

newQueue :: IO (TBQueue a)
newQueue = newTBQueueIO qUEUE_SIZE

newDeployment :: IO (Deployment ())
newDeployment = Deployment <$> newTBQueueIO 65536 <*> pure []

asyncP :: Pipeline IO a b -> TBQueue a -> IO (TBQueue b)
asyncP (ArrM f)       q = do
  q' <- newQueue
  _a  <- async (forever (atomically (readTBQueue q) >>= f >>= atomically . writeTBQueue q'))
  return q'
asyncP (Sequence f g) q = do
  q' <- asyncP f q
  asyncP g q'
asyncP (Combine f g) q = do
  q'   <- asyncP f q
  q''  <- asyncP g q
  q''' <- newQueue
  _a <- async (forever (atomically (readTBQueue q' <|> readTBQueue q'') >>= atomically . writeTBQueue q'''))
  return q'''
asyncP (Par _f _g) _q = do
  -- q'   <- asyncP f q1
  -- q''  <- asyncP g q2
  undefined
  -- XXX: use type families to get two queues in and out a la vector
asyncP _ _ = error "TODO"

deploy :: Pipeline IO () () -> IO ()
deploy p = do
  q <- newTBQueueIO 1
  withAsync (forever (atomically (writeTBQueue q ()))) $ \a -> do
    void (asyncP p q)
    wait a

tempDeploy :: Pipeline IO () () -> Int -> IO ()
tempDeploy p ticks = do
  q <- newTBQueueIO 1
  withAsync (replicateM_ ticks (atomically (writeTBQueue q ()))) $ \a -> do
    void (asyncP p q)
    wait a

simple :: Pipeline IO () ()
simple = (ArrM (const (randomSleep >> return "hi")) <+> ArrM (const (randomSleep >> return "bye"))) >>> ArrM putStrLn

simple' :: Pipeline IO () ()
simple' = (ArrM (const (randomSleep >> return "hi")) <+> ArrM (const (randomSleep >> return "bye"))) >>>
  Fork >>> (ArrM putStrLn `Par` ArrM print) >>> ConsumeP

randomSleep :: IO ()
randomSleep = do
  millis <- randomRIO (10, 1000)
  threadDelay (millis * 1000)

example :: Pipeline IO () ()
example = receiver >>> parser >>> right (Lift logic) >>> responder
  where
    receiver :: Pipeline IO () ByteString
    receiver = undefined

    parser :: Pipeline IO ByteString (Either String Int)
    parser = undefined

    logic :: FreeFunc Int Int
    logic = undefined

    responder :: Pipeline IO (Either String Int) ()
    responder = undefined

embed :: Monad m => [a] -> Pipeline m a b -> m [b]
embed is p = traverse (runPipeline p) is

reactimate :: Monad m => Pipeline m () () -> m ()
reactimate p = forever (runPipeline p ())

network :: TBQueue ByteString -> Pipeline IO () ByteString
network queue = proc _ ->
  ArrM (atomically . readTBQueue) -< queue

webserver :: IO ()
webserver = do
  queue <- newTBQueueIO 65536
  withAsync (undefined queue) $ \_a -> do
    undefined
