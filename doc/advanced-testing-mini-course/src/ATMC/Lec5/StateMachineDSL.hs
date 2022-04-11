{-# LANGUAGE TemplateHaskell #-}

module ATMC.Lec5.StateMachineDSL
  ( module ATMC.Lec5.StateMachineDSL
  , module Lens.Micro.Platform
  )
where

import Lens.Micro.Platform
import Control.Monad.Trans.Class
import Control.Monad.Trans.State hiding (get)
import Control.Monad.Trans.Writer

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

data ExampleState = ExampleState
  { _esInt :: Int
  }
  deriving (Eq, Show)

makeLenses ''ExampleState

initExState :: ExampleState
initExState = ExampleState 0

data Req = Req
  deriving (Eq, Show)

data Msg = Msg
  deriving (Eq, Show)

data Resp = Resp Int
  deriving (Eq, Show)

example :: Input Req Msg -> SMM ExampleState Msg Resp ()
example (ClientRequest at cid req) = do
  esInt .= 1
  esInt += 2
  esInt += 3
  s <- use esInt
  respond cid (Resp s)

t :: Bool
t = runSMM (example (ClientRequest epoch (ClientId 0) Req)) initExState
    == ([ClientResponse (ClientId 0) (Resp 6)],ExampleState {_esInt = 6})

sm :: SM ExampleState Req Msg Resp
sm = SM initExState (runSMM . example)
