module StuntDouble.Random
  ( Seed
  , RandomInterval
  , makeSeed
  , makeSeedIO
  , interval
  , exponential
  , add
  , detRandomInterval
  , isLessThan
  )
  where

import Data.Fixed (mod')
import System.Random

------------------------------------------------------------------------

newtype Seed = Seed StdGen

instance RandomGen Seed where
  split (Seed g) = let (g', g'') = split g in (Seed g', Seed g'')
  next  (Seed g) = fmap Seed (next g)

makeSeed :: Int -> Seed
makeSeed = Seed . mkStdGen

makeSeedIO :: IO Seed
makeSeedIO = fmap makeSeed randomIO

newtype RandomInterval = RandomInterval Double
  deriving Show

detRandomInterval :: Double -> RandomInterval
detRandomInterval d = RandomInterval $ mod' d 1

add :: RandomInterval -> RandomInterval -> RandomInterval
add (RandomInterval d) (RandomInterval d') = detRandomInterval $ d + d'

isLessThan :: RandomInterval -> Double -> Bool
isLessThan (RandomInterval d) d' = d < d'

interval :: Seed -> (RandomInterval, Seed)
interval seed =
  let
    (d, seed') = randomR (0, 1) seed
  in
    (RandomInterval d, seed')

exponential :: Double -> RandomInterval -> Double
exponential mean (RandomInterval u) = (- mean) * log u
