-- Deconstructing Lambdas - An Awkward Guide to Programming Without Functions:
--   https://www.youtube.com/watch?v=xZmPuz9m2t0


-- http://blog.sigfpe.com/2011/07/profunctors-in-haskell.html
-- http://blog.sigfpe.com/2017/01/addressing-pieces-of-state-with.html
-- http://blog.sigfpe.com/2017/01/building-free-arrows-from-components.html

-- https://github.com/kowainik/prolens

-- https://old.reddit.com/r/haskell/comments/tsk9wn/alternative_to_arrows/
-- https://hackage.haskell.org/package/overloaded-0.3.1/docs/Overloaded-Categories.html

newtype SM s t i o = SM { runSM :: (i, s) -> (o, t) }

instance Category (SM s s) where
  id = SM id
  SM f . SM g = SM (f . g)

instance Profunctor (SM s s)

instance CartesianCategory

instance Strong

class Primitives k where
  .. k a b

instance Primitives (->)

instance Primitives SM

ex :: (Primitives k, Cartesian k, Strong k) => k String Bool
ex = undefined

-- > :t ex @SM

newtype Diagram = Diagram (State Graph InputOutputLinks)

-- > renderDiagram (ex @Diagram)

data FreeFunc p a b
  Id  :: FreeFunc p x x
  ..
  Lift p a b -> FreeFunc p a b

instance Category (FreeFunc k)
instance Cartesian (FreeFunc k)
instance Strong (FreeFunc k)


data Prims a b

instance Primitives (FreeFunc Prims) w

> show (ex @(FreeFunc Prims)

read
