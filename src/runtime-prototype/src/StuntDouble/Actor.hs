{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}

module StuntDouble.Actor where

import Control.Concurrent.Async

import StuntDouble.Message
import StuntDouble.Reference
import StuntDouble.FreeMonad

------------------------------------------------------------------------

type Actor = Free ActorF (Cont Message)

data Cont a
  = Now a
  | Later (Async a) (a -> Actor)

  -- Sketch of later extension:

  -- Later [Async a] Strategy ([Either Exception a] -> Actor)
  -- data Strategy
  --   = Any -- ^ call the continuation as soon as any of the asyncs finishes (or succeeds?).
  --   | All -- ^ call the continuation when all asyncs finish.
  --   | Atleast Int
  --   | ...

-- XXX: dup
data IOResult = Unit | String String
  deriving Show

data ActorF x
  = Call LocalRef Message (Message -> x)
  | RemoteCall RemoteRef Message (Async Message -> x)
  | AsyncIO (IO IOResult) (Async IOResult -> x)
  -- | On [(Async a)] Strategy ([a] -> x) (() -> x)
  | Get (State -> x)
  | Put State (() -> x)
deriving instance Functor ActorF

on :: Async a -> (a -> Free ActorF ()) -> Free ActorF ()
on = undefined

call' :: LocalRef -> Message -> Free ActorF Message
call' lr m = Free (Call lr m return)

remoteCall :: RemoteRef -> Message -> Free ActorF (Async Message)
remoteCall rr m = Free (RemoteCall rr m return)

asyncIO :: IO IOResult -> Free ActorF (Async IOResult)
asyncIO m = Free (AsyncIO m return)


-- XXX:
newtype State = State Int
  deriving Num

exampleActor :: LocalRef -> RemoteRef -> Message -> Actor
exampleActor lref rref msg = do
  state <- Free (Get return)
  reply <- Free (Call lref (Message "help") return)
  Free (Put 1 return)
  a <- Free (RemoteCall rref (Message ("got: " ++ show msg)) return)
  return $ Later a $ \msg -> do
    Free (Put 2 return)
    return (Now msg)

actorA :: RemoteRef -> Message -> Actor
actorA bref (Message "init") = do
  a <- Free (RemoteCall bref (Message "hi") return)
  return (Later a (\reply -> return (Now reply)))

actorB :: RemoteRef -> Message -> Actor
actorB aref (Message "hi") = do
  -- a <- Free (RemoteCall bref (Message "init") return)
  -- return (Later a (\reply -> return (Now (Message "bye"))))
  undefined
