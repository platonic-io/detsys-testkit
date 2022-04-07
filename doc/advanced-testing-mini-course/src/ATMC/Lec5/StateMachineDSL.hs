{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module ATMC.Lec5.StateMachineDSL where

import Control.Monad.Trans.Class
import Control.Monad.Trans.State hiding (get)
import Control.Monad.Trans.Writer
import GHC.Records.Compat

import ATMC.Lec5.StateMachine
import ATMC.Lec5.Time

------------------------------------------------------------------------

type SMM s msg resp a = StateT s (Writer [Output resp msg]) a

runSMM :: SMM s msg resp () -> s -> ([Output resp msg], s)
runSMM m s = (outputs, s')
  where
    (((), s'), outputs) = runWriter (runStateT m s)

send :: NodeId -> msg -> SMM s msg resp ()
send nid msg = lift (tell [InternalMessageOut nid msg])

respond :: ClientId -> resp -> SMM s msg resp ()
respond cid resp = lift (tell [ClientResponse cid resp])

set :: forall f s a msg resp. HasField f s a
    => a -> SMM s msg resp ()
set x = modify (\s -> setField @f s x)

get :: forall f s a msg resp. HasField f s a
    => SMM s msg resp a
get = gets (getField @f)

update :: forall f s a msg resp. HasField f s a
       => (a -> a) -> SMM s msg resp ()
update u = modify (\s -> setField @f s (u (getField @f s)))

data ExampleState = ExampleState
  { esInt :: Int
  }
  deriving (Eq, Show)

initExState :: ExampleState
initExState = ExampleState 0

instance HasField "esInt" ExampleState Int where
  hasField (ExampleState i) = (ExampleState, i)

data Req = Req
  deriving (Eq, Show)

data Msg = Msg
  deriving (Eq, Show)

data Resp = Resp Int
  deriving (Eq, Show)

example :: Input Req Msg -> SMM ExampleState Msg Resp ()
example (ClientRequest at cid req) = do
  set    @"esInt" 1
  update @"esInt" (+2)
  update @"esInt" (+3)
  s <- get @"esInt"
  respond cid (Resp s)

t :: Bool
t = runSMM (example (ClientRequest epoch (ClientId 0) Req)) initExState
    == ([ClientResponse (ClientId 0) (Resp 6)],ExampleState {esInt = 6})

sm :: SM ExampleState Req Msg Resp
sm = SM initExState (runSMM . example)
