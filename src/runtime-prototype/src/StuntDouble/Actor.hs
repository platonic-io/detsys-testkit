{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module StuntDouble.Actor where

import Control.Concurrent.Async
import Control.Monad.Free

import StuntDouble.Message
import StuntDouble.Reference

------------------------------------------------------------------------

type Actor = Free ActorF Message

data ActorF x
  = Call LocalRef Message (Message -> x)
  | RemoteCall RemoteRef Message (Async Message -> x)
  | On (Async Message) (Message -> Free ActorF ()) (() -> x)
  | Get (State -> x)
  | Put State (() -> x)
  deriving Functor

-- XXX:
newtype State = State Int
  deriving Num

exampleActor :: LocalRef -> RemoteRef -> Message -> Actor
exampleActor lref rref msg = do
  state <- Free (Get return)
  reply <- Free (Call lref (Message "help") return)
  Free (Put 1 return)
  a <- Free (RemoteCall rref (Message ("got: " ++ show msg)) return)
  Free (On a (\_msg -> Free (Put 2 return)) return)
  return (Message "reply")
