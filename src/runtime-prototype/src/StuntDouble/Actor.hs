{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE StandaloneDeriving #-}

module StuntDouble.Actor where

import Control.Concurrent.Async
import Data.Text (Text)
import Data.Time

import StuntDouble.Message
import StuntDouble.Reference
import StuntDouble.FreeMonad
import StuntDouble.Actor.State
import StuntDouble.Datatype

------------------------------------------------------------------------

-- XXX: use newtype and avoid flexible instances for monad fail...
type Actor = Free ActorF (Cont Message)
instance MonadFail (Free ActorF) where
  fail = error

data Cont a
  = Now a
  | Later (Async (Maybe a)) (a -> Actor)
  | LaterIO (Async IOResult) (IOResult -> Actor)

  -- Sketch of later extension:

  -- Later [Async a] Strategy ([Either Exception a] -> Actor)
  -- data Strategy
  --   = Any -- ^ call the continuation as soon as any of the asyncs finishes (or succeeds?).
  --   | All -- ^ call the continuation when all asyncs finish.
  --   | Atleast Int
  --   | ...

data IOResult = IOUnit | String String
  deriving (Eq, Ord, Show)

data ActorF x
  = Call LocalRef Message (Message -> x)
  | RemoteCall RemoteRef Message (Async (Maybe Message) -> x)
  | AsyncIO (IO IOResult) (Async IOResult -> x)
  -- | On [(Async a)] Strategy ([a] -> x) (() -> x)
  -- | On (Async IOResult) (IOResult -> x) (() -> x)
  | On    (Either (Async Message) (Async IOResult)) (Either Message IOResult -> x) (() -> x)
  | UnsafeAwait (Either (Async (Maybe Message)) (Async IOResult)) (Either (Maybe Message) IOResult -> x)
  | Get (State -> x)
  | Put State (() -> x)
  -- | Throw Reason (Void -> x)
deriving instance Functor ActorF

on :: Async a -> (a -> Free ActorF ()) -> Free ActorF ()
on = undefined

call :: LocalRef -> Message -> Free ActorF Message
call lr m = Free (Call lr m return)

remoteCall :: RemoteRef -> Message -> Free ActorF (Async (Maybe Message))
remoteCall rr m = Free (RemoteCall rr m return)

unsafeAwait :: Either (Async (Maybe Message)) (Async IOResult)
            -> Free ActorF (Either (Maybe Message) IOResult)
unsafeAwait a = Free (UnsafeAwait a return)

asyncIO :: IO IOResult -> Free ActorF (Async IOResult)
asyncIO m = Free (AsyncIO m return)

getState :: Free ActorF State
getState = Free (Get return)

putState :: State -> Free ActorF ()
putState state' = Free (Put state' return)

------------------------------------------------------------------------

class Convertible a where
  convertFrom :: a -> Datatype
  convertTo   :: Datatype -> a

instance Convertible Integer where
  convertFrom = Integer
  convertTo (Integer i) = i

instance Convertible Text where
  convertFrom = Text
  convertTo (Text t) = t

instance Convertible UTCTime where
  convertFrom = Timestamp
  convertTo (Timestamp t) = t

instance (Convertible a, Convertible b) => Convertible (a, b) where
  convertFrom (x, y)     = Pair (convertFrom x) (convertFrom y)
  convertTo   (Pair x y) = (convertTo x, convertTo y)

instance Convertible a => Convertible (Maybe a) where
  convertFrom Nothing = None
  convertFrom (Just x) = Some (convertFrom x)

  convertTo None = Nothing
  convertTo (Some x) = Just (convertTo x)

instance Convertible Datatype where
  convertFrom = id
  convertTo = id

(^.) :: Convertible a => Text -> (Datatype -> Datatype) -> Free ActorF a
k ^. a = do
  s <- getState
  return (convertTo (a (getField k s)))

(.=) :: Convertible a => Text -> a -> Free ActorF ()
k .= v = do
  s <- getState
  putState (setField k (convertFrom v) s)

(%=) :: (Datatype -> Datatype) -> Text -> Free ActorF ()
f %= k = do
  s <- getState
  putState (modifyField k f s)

op2 :: (Convertible a, Convertible b)
    => (Datatype -> Datatype -> Datatype -> Datatype) -> a -> b -> (Datatype -> Datatype)
op2 f x y = f (convertFrom x) (convertFrom y)

get :: Convertible a => Text -> Free ActorF a
get k = do
  s <- getState
  return (convertTo (getField k s))

genArrivalTime :: UTCTime -> Integer -> Free ActorF UTCTime
genArrivalTime = undefined
