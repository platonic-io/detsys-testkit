{-# LANGUAGE Arrows #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Lec09.Pipeline where

import Control.Applicative
import qualified Control.Arrow as Arrow
import Control.Category (Category, id, (.), (>>>))
import Control.Concurrent
import Data.Functor.Identity
import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Monad
import Data.ByteString.Lazy (ByteString)
import Data.Kind (Type)
import Numeric.Natural
import Prelude hiding (id, (.))
import System.Random

import Lec09.FreeFunc (FreeFunc, interpret)

------------------------------------------------------------------------

data Universe = Universe :* Universe | Universe :+ Universe | K Type

data Pipeline m a b where
  -- ArrM     :: (Element a -> m (Element b)) -> Pipeline m a b
  Id       :: Pipeline m a a
  Lift     :: SM m a b -> Pipeline m (K a) (K b)
  ConsumeP :: Pipeline m a (K ())
  Sequence :: Pipeline m a b -> Pipeline m b c -> Pipeline m a c
  Shard    :: Pipeline m (K a) (K b) -> Pipeline m (K a) (K b)
  -- Tee ?
  Fork     :: Pipeline m a (a :* a)
  Fst      :: Pipeline m (a :* b) a
  Snd      :: Pipeline m (a :* b) b
  Par      :: Pipeline m a c -> Pipeline m b d -> Pipeline m (a :* b) (c :* d)
  -- Split    :: (a -> Bool) -> Pipeline m a (Either a a)
  Or       :: Pipeline m a c -> Pipeline m b d -> Pipeline m (a :+ b) (c :+ d)
  Eq :: Eq (Element a) => Pipeline m (a :* a) (K Bool)

instance Monad m => Category (Pipeline m) where
  id    = Id
  g . f = Sequence f g

--instance Monad m => Arrow (Pipeline m) where
--  arr   = ArrM . (return .)
--  (***) = Par
--
--  (&&&) :: Pipeline m a b -> Pipeline m a c -> Pipeline m a (b, c)
--  f &&& g = Fork >>> f `Par` g
--
--instance Monad m => ArrowChoice (Pipeline m) where
--  (+++) :: Pipeline m a c -> Pipeline m b d -> Pipeline m (Either a b) (Either c d)
--  (+++) = Or
--
--  (|||) :: Pipeline m b d -> Pipeline m c d -> Pipeline m (Either b c) d
--  f ||| g = f +++ g >>> arr untag
--    where
--      untag (Left  x) = x
--      untag (Right y) = y
--
--instance (Monad m, MonadPlus m) => ArrowZero (Pipeline m) where
--  zeroArrow = ArrM (const mzero)
--
--instance (Monad m, MonadPlus m) => ArrowPlus (Pipeline m) where
--  (<+>) = Combine

------------------------------------------------------------------------

type family Element a where
  Element (a :* b)  = (Element a, Element b)
  Element (a :+ b)  = Either (Element a) (Element b)
  Element (K a)     = a

data SM m i o = forall s. SM
  { smLoad  :: m s
  , smStore :: s -> m ()
  , smStep  :: FreeFunc (i, s) (o, s)
  }

runPipeline :: Monad m => Pipeline m a b -> Element a -> m (Element b)
runPipeline Id                  x         = return x
runPipeline (Sequence f g)      x         = runPipeline f x >>= runPipeline g
runPipeline (Par f g)           (l, r)    = (,) <$> runPipeline f l <*> runPipeline g r
runPipeline ConsumeP            _x        = return ()
runPipeline (Or f _g)           (Left l)  = Left  <$> runPipeline f l
runPipeline (Or _f g)           (Right r) = Right <$> runPipeline g r
runPipeline Fork                x         = return (x, x)
runPipeline Fst                 (x, _y)   = return x
runPipeline Snd                 (_x, y)   = return y
runPipeline (Lift (SM ld st f)) i         = do
  s <- ld
  let (o, s') = interpret f (i, s)
  st s'
  return o
runPipeline Eq                  (x, y)    = return (x == y)

listPipeline :: Monad m => Pipeline m a b -> [Element a] -> m [Element b]
listPipeline p = traverse (runPipeline p)

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

type family Queue u where
  Queue (l ':* r)  = (Queue l, Queue r)
  Queue (l ':+ r)  = (Queue l, Queue r)
  Queue ('K a)     = TBQueue a

asyncP :: Pipeline IO a b -> Queue a -> IO (Queue b)
asyncP Id             q = return q
asyncP (Sequence f g) q = do
  q' <- asyncP f q
  asyncP g q'
-- asyncP (Combine f g) q = do
--  q'   <- asyncP f q
--  q''  <- asyncP g q
--  q''' <- newQueue
--  _a <- async (forever (atomically (readTBQueue q' <|> readTBQueue q'') >>= atomically . writeTBQueue q'''))
--  return q'''
asyncP (Par f g) (q1, q2) = do
  q'   <- asyncP f q1
  q''  <- asyncP g q2
  return (q', q'')
asyncP (Or f g) q = undefined
asyncP (Shard f) q = do
  q1 <- newQueue
  q2 <- newQueue
  qr1 <- asyncP f q1
  qr2 <- asyncP f q2
  qr <- newQueue
  _ <- async $ shardQIn q q1 q2
  _ <- async $ shardQOut qr1 qr2 qr
  return qr
  where
    shardQIn from to extra = atomically (readTBQueue from) >>= atomically . writeTBQueue to >> shardQIn from extra to
    shardQOut from extra to = atomically (readTBQueue from) >>= atomically . writeTBQueue to >> shardQOut extra from to

(&&&) :: Monad m => Pipeline m a b -> Pipeline m a c -> Pipeline m a (b :* c)
f &&& g = Fork >>> f `Par` g

swap :: Monad m => Pipeline m (a :* b) (b :* a)
swap = Snd &&& Fst

testJoin :: Pipeline IO (a :* b) c -> (Queue a, Queue b) -> IO (Queue c)
testJoin p = asyncP p

testJoin' :: (TBQueue Int, TBQueue Int) -> IO (TBQueue Bool)
testJoin' = testJoin p
  where
    p :: Pipeline IO (K Int :* K Int) (K Bool)
    p = Eq

data Producer a b = Producer
  { rProduce      :: IO a -- (a, MVar b)
  , rBackpressure :: IO ()
  }

data Consumer b = Consumer
  { rConsume :: b -> IO () -- (b, MVar b) -> IO ()
  }

data Service = forall a b. Service (Producer a b) (Pipeline IO (K a) (K b)) (Consumer b)

deploy :: Service -> IO ()
deploy (Service producer pipeline consumer) = do
  qa <- newQueue
  -- qb <- asyncP (first pipeline) qa
  qb <- asyncP pipeline qa
  withAsync (forever (rProduce producer >>= atomically . writeTBQueue qa)) $ \_a ->
    forever (atomically (readTBQueue qb) >>= rConsume consumer)

first :: Monad m => Pipeline m a b -> Pipeline m (a :* x) (b :* x)
first f = f `Par` Id

-- deploy :: Pipeline IO () () -> IO ()
-- deploy p = do
--   q <- newTBQueueIO 1
--   withAsync (forever (atomically (writeTBQueue q ()))) $ \a -> do
--     void (asyncP p q)
--     wait a
--
-- tempDeploy :: Pipeline IO () () -> Int -> IO ()
-- tempDeploy p ticks = do
--   q <- newTBQueueIO 1
--   withAsync (replicateM_ ticks (atomically (writeTBQueue q ()))) $ \a -> do
--     void (asyncP p q)
--     wait a
--
-- simple :: Pipeline IO () ()
-- simple = (ArrM (const (randomSleep >> return "hi")) <+> ArrM (const (randomSleep >> return "bye"))) >>> ArrM putStrLn
--
-- simple' :: Pipeline IO () ()
-- simple' = (ArrM (const (randomSleep >> return "hi")) <+> ArrM (const (randomSleep >> return "bye"))) >>>
--   Fork >>> (ArrM putStrLn `Par` ArrM print) >>> ConsumeP
--
-- randomSleep :: IO ()
-- randomSleep = do
--   millis <- randomRIO (10, 1000)
--   threadDelay (millis * 1000)
--
-- example :: Pipeline IO () ()
-- example = receiver >>> parser >>> right (Lift logic) >>> responder
--   where
--     receiver :: Pipeline IO () ByteString
--     receiver = undefined
--
--     parser :: Pipeline IO ByteString (Either String Int)
--     parser = undefined
--
--     logic :: FreeFunc Int Int
--     logic = undefined
--
--     responder :: Pipeline IO (Either String Int) ()
--     responder = undefined
--
-- embed :: Monad m => [a] -> Pipeline m a b -> m [b]
-- embed is p = traverse (runPipeline p) is
--
-- reactimate :: Monad m => Pipeline m () () -> m ()
-- reactimate p = forever (runPipeline p ())
--
-- webserver :: IO ()
-- webserver = do
--   queue <- newTBQueueIO 65536
--   withAsync (undefined queue) $ \_a -> do
--     undefined
