module StuntDouble.Random where

import System.Random

------------------------------------------------------------------------

data Seed = Seed StdGen

makeSeed :: Int -> Seed
makeSeed = Seed . mkStdGen

uniform :: Seed -> (Double, Seed)
uniform (Seed g) = fmap Seed (randomR (0, 1) g)
