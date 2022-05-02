{-# LANGUAGE TemplateHaskell #-}

module ATMC.Lec05.StateMachineDSL
  ( module ATMC.Lec05.StateMachineDSL
  , module Lens.Micro.Platform
  )
where

import Data.Fixed
import System.Random (StdGen, mkStdGen)
import qualified System.Random as Random
import Lens.Micro.Platform
import Control.Monad.Trans.Class
import Control.Monad.Trans.State
import Control.Monad.Trans.Writer
import Control.Monad.Trans.Cont

import ATMC.Lec05.StateMachine
import ATMC.Lec05.Time

------------------------------------------------------------------------

type SMM s msg resp a =
  ContT Guard (StateT s (StateT StdGen (Writer [Output resp msg]))) a

data Guard = Keep | Drop

runSMM :: SMM s msg resp () -> s -> StdGen -> ([Output resp msg], s, StdGen)
runSMM m s gen =
  case runWriter (runStateT (runStateT (runContT m (const (return Keep))) s) gen) of
    (((Keep,  s'),  gen'),  outputs) -> (outputs, s', gen')
    (((Drop, _s'), _gen'), _outputs) -> ([], s, gen)

send :: NodeId -> msg -> SMM s msg resp ()
send nid msg = lift (lift (lift (tell [InternalMessageOut nid msg])))

respond :: ClientId -> resp -> SMM s msg resp ()
respond cid resp = lift (lift (lift (tell [ClientResponse cid resp])))

registerTimerSeconds :: Pico -> SMM s msg resp ()
registerTimerSeconds secs = lift (lift (lift (tell [RegisterTimerSeconds secs])))

resetTimerSeconds :: Pico -> SMM s msg resp ()
resetTimerSeconds secs = lift (lift (lift (tell [ResetTimerSeconds secs])))

ereturn :: SMM s msg resp a
ereturn = ContT (const (return Keep))

guard :: Bool -> SMM s msg resp ()
guard True  = return ()
guard False = ContT (const (return Drop))

guardM :: SMM s msg resp Bool -> SMM s msg resp ()
guardM m = do
  b <- m
  guard b

random :: SMM s msg resp Int
random = do
  g <- lift (lift get)
  let (i, g') = Random.random g
  lift (lift (put g'))
  return i

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
t = runSMM (example (ClientRequest epoch (ClientId 0) Req)) initExState (mkStdGen 0)
    == ([ClientResponse (ClientId 0) (Resp 6)], ExampleState {_esInt = 6}, (mkStdGen 1))

sm :: SM ExampleState Req Msg Resp
sm = SM initExState (\i s g -> runSMM (example i) s g) noTimeouts
