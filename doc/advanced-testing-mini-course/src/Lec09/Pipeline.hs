{-# LANGUAGE Arrows #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Lec09.Pipeline where

import Control.Applicative
import qualified Control.Arrow as Arrow
import Control.Category (Category, id, (.), (>>>))
import Control.Concurrent
import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Monad
import Control.Monad.Trans.State
import Data.IORef
import Data.Vector.Mutable (IOVector)
import qualified Data.Vector.Mutable as Vector
import Data.ByteString.Lazy (ByteString)
import Data.Functor.Identity
import Network.HTTP.Types.Status
import Data.Kind (Type)
import Network.Wai
import Network.Wai.Handler.Warp
import Numeric.Natural
import Prelude hiding (id, (.))
import System.Random

import Lec09.FreeFunc (FreeFunc, interpret)

------------------------------------------------------------------------

data Universe = Universe :+ Universe | Universe :* Universe | K Type | Type :& Type

data Pipeline m a b where
  -- ArrM     :: (Element a -> m (Element b)) -> Pipeline m a b
  Id       :: Pipeline m a a
  Lift     :: SM m a b -> Pipeline m (K a) (K b)
  ConsumeP :: Pipeline m a (K ())
  Sequence :: Pipeline m a b -> Pipeline m b c -> Pipeline m a c
  Shard    :: Pipeline m (K a) (K b) -> Pipeline m (K a) (K b)
  -- Tee ?
  Fork     :: Pipeline m a (a :* a)
  Join     :: Pipeline m (K a :* K b) (K (a, b))
  Fst      :: Pipeline m (a :* b) a
  Snd      :: Pipeline m (a :* b) b
  Par      :: Pipeline m a c -> Pipeline m b d -> Pipeline m (a :* b) (c :* d)
  First    :: Pipeline m (K a) (K b) -> Pipeline m (a :& x) (b :& x)
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
  Element (a :& b)  = (a, b)
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
runPipeline Join                (x, y)    = return (x, y)
runPipeline Fst                 (x, _y)   = return x
runPipeline Snd                 (_x, y)   = return y
runPipeline (First f)           (x, y)    = runPipeline f x >>= \x' -> return (x', y)
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
  Queue (l ':& r)  = TBQueue (l, r)
  Queue (l ':+ r)  = (Queue l, Queue r)
  Queue ('K a)     = TBQueue a

type family Q q a where
  Q q (a :* b)  = (Q q a, Q q b)
  Q q (a :& b)  = q (a, b)
  Q q (a :+ b)  = (Q q a, Q q b)
  Q q (K a)     = q a

asyncP :: Pipeline IO a b -> Q TBQueue a -> IO (Q TBQueue b)
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
asyncP (First f) q = do
  q' <- newQueue
  q'' <- asyncP f q'
  qp <- newQueue
  _a <- async $ forever $ atomically $ do
    (x, y) <- readTBQueue q
    writeTBQueue q' x
    x' <- readTBQueue q''
    writeTBQueue qp (x', y)
  return qp
asyncP Fork q = do
  q1 <- newQueue
  q2 <- newQueue
  _ <- async $ forever $ atomically $ do
    x <- undefined
    writeTBQueue q1 x
    writeTBQueue q2 x
  undefined -- return (q1, q2)
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

testJoin :: (TBQueue Int, TBQueue Int) -> IO (TBQueue Bool)
testJoin = asyncP p
  where
    p :: Pipeline IO (K Int :* K Int) (K Bool)
    p = Eq

data Producer a b = Producer
  { rProduce      :: IO (a, MVar b)
  , rBackpressure :: IO ()
  , rRunProducer  :: IO ()
  }

warpProducer :: IO (Producer ByteString ByteString)
warpProducer = do
  queue <- newQueue
  return Producer
    { rProduce      = atomically (readTBQueue queue)
    , rBackpressure = undefined
    , rRunProducer  = run 8050 (app queue)
    }
  where
    app queue req respond = do
      -- XXX: if backpressure then respond 503 for a while...
      body <- consumeRequestBodyLazy req
      mvar <- newEmptyMVar
      atomically (writeTBQueue queue (body, mvar))
      resp <- takeMVar mvar
      respond (responseLBS status200 [] resp)

data Consumer b = Consumer
  { rConsume :: (b, MVar b) -> IO ()
  }

warpConsumer :: Consumer ByteString
warpConsumer = Consumer
  { rConsume = (\(y, mvar) -> putMVar mvar y) }

data Service = forall a b. Service (Producer a b) (Pipeline IO (K a) (K b)) (Consumer b)

smCounter :: SM IO ByteString ByteString
smCounter = SM undefined undefined undefined


echoPipeline :: Pipeline IO (K ByteString) (K ByteString)
echoPipeline = Lift smCounter

warpService :: IO Service
warpService = do
  producer <- warpProducer
  return (Service producer echoPipeline warpConsumer)

deploy :: Service -> IO ()
deploy (Service producer pipeline consumer) = do
  qa <- newQueue
  withAsync (rRunProducer producer) $ \_a -> do
    qb <- asyncP (First pipeline) qa -- Doesn't work because it splits up the queue into two...
    withAsync (forever (rProduce producer >>= atomically . writeTBQueue qa)) $ \_a ->
      forever (atomically (readTBQueue qb) >>= rConsume consumer)

main :: IO ()
main = do
  service <- warpService
  deploy service

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

------------------------------------------------------------------------

data P a b where
  Arr   :: (a -> b) -> P a b
  Seq   :: P a b -> P b c -> P a c
  Fork' :: P a b -> P a c -> P a (Either b c)
  Join' :: P (Either a a) a

data RB a = RB
  { rbVec   :: IOVector a
  , rbSeqNr :: IORef Int
  , rbGates :: IORef [IORef Int]
  }

newRB :: IORef Int -> IO (RB a)
newRB seqNr = RB <$> Vector.new 16 <*> pure seqNr <*> newIORef []

addGate :: RB a -> IORef Int -> IO ()
addGate rb gate = modifyIORef (rbGates rb) (gate :)

rbRead :: RB a -> Int -> IO a
rbRead rb ix = Vector.read (rbVec rb) ix

rbWrite :: RB a -> Int -> a -> IO ()
rbWrite rb ix x = do
  Vector.write (rbVec rb) ix x
  modifyIORef (rbSeqNr rb) (+1)

rbModify :: RB a -> (a -> a) -> Int -> IO ()
rbModify rb f ix = Vector.modify (rbVec rb) f ix


d :: P a b -> RB a -> IO (RB b)
d (Arr f) rb = do
  consumedSeqNr <- newIORef 0
  rb' <- newRB consumedSeqNr
  _ <- async $ forever $ do
    consumed <- readIORef consumedSeqNr
    produced <- readIORef (rbSeqNr rb)
    go rb' consumed (produced - 1)
    writeIORef consumedSeqNr produced
    threadDelay 1000
  addGate rb consumedSeqNr
  return rb'
  where
    go rb' lo hi | lo > hi   = return ()
                 | otherwise = do
                     x <- rbRead rb lo
                     rbWrite rb' lo (f x)
                     go rb' (lo + 1) hi
d (Seq f g) rb = d f rb >>= d g

data S a b = S (Source a) (P a b) (Sink b)

data Source a = Source (IO a)

data Sink a = Sink (a -> IO ())

listSource :: [a] -> IO (Source a)
listSource xs = do
  ref <- newIORef xs
  return (Source (threadDelay 1000000 >> atomicModifyIORef' ref (\xs -> (tail xs, head xs))))

printSink :: Show a => Sink a
printSink = Sink print

ds :: S a b -> IO ()
ds (S (Source source) pipeline (Sink sink)) = do
  sourceSeqNr <- newIORef 0
  rb <- newRB sourceSeqNr
  _ <- async $ forever $ do
    x <- source
    ix <- readIORef sourceSeqNr
    rbWrite rb ix x
  rb' <- d pipeline rb
  sinkSeqNr <- newIORef 0
  forever $ do
    produced <- readIORef (rbSeqNr rb')
    sinked <- readIORef sinkSeqNr
    go rb' sinked (produced - 1)
    writeIORef sinkSeqNr produced
    threadDelay 10000
  where
    go rb lo hi | lo > hi   = return ()
                | otherwise = do
                    x <- rbRead rb lo
                    sink x
                    go rb (lo + 1) hi

t :: IO ()
t = do
  source <- listSource [1..10]
  ds (S source (Arr (+ 1) `Seq` Arr (* 2)) printSink)

------------------------------------------------------------------------

data SP a b where
  A :: (a -> b) -> SP a b
  (:>>) :: SP a b -> SP b c -> SP a c

  SFork :: SP a b -> SP a c -> SP a (b, c)
  SShard :: SP a a -> SP a a -> SP a a

  Src :: IO a -> SP () a
  Plus :: SP () a -> SP () a -> SP () a

  SSink :: (a -> IO ()) -> SP a ()
  Tee :: SP a () -> SP a a

list :: [a] -> IO (IO a)
list xs = do
  ref <- newIORef xs
  return (atomicModifyIORef' ref (\xs -> (tail xs, head xs)))

s :: IO (SP () ())
s = do
  src <- list [1..10]
  return $ Src src :>> SFork (A (+ 1)) (A (* 2)) :>> Tee (SSink (\x -> threadDelay 1000000 >> print x)) :>> SSink print

sp :: SP a b -> TBQueue a -> IO (TBQueue b)
sp (A f)        xs = do
  ys <- newQueue
  _ <- async $ forever $ atomically $ do
    x <- readTBQueue xs
    writeTBQueue ys (f x)
  return ys
sp (f :>> g)    xs = sp f xs >>= sp g
sp (SFork l r)  xs = do
  xsl <- newQueue
  xsr <- newQueue
  _ <- async $ forever $ atomically $ do
    x <- readTBQueue xs
    writeTBQueue xsl x
    writeTBQueue xsr x

  ysl <- sp l xsl
  ysr <- sp r xsr

  lrs <- newQueue

  _ <- async $ forever $ atomically $ do
    yl <- readTBQueue ysl
    yr <- readTBQueue ysr
    writeTBQueue lrs (yl, yr)

  return lrs

sp (SShard e o) xs = undefined
sp (Src io) _xs = do
  xs <- newQueue
  _ <- async $ forever $ do
    x <- io
    atomically $ writeTBQueue xs x
  return xs
sp (SSink k) xs = do
  ys <- newQueue
  _ <- async $ forever $ do
    x <- atomically $ readTBQueue xs
    k x
    atomically $ writeTBQueue ys ()
  return ys
sp (Tee sink) xs = do
  xsl <- newQueue
  xsr <- newQueue
  _ <- async $ forever $ atomically $ do
    x <- readTBQueue xs
    writeTBQueue xsl x
    writeTBQueue xsr x
  sp sink xsl
  return xsr

m :: IO ()
m = do
  pipe <- s
  xs <- newQueue
  ys <- sp pipe xs
  replicateM_ 10 (atomically (readTBQueue ys))
  return ()
