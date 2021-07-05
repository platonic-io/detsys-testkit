module StuntDouble.Random where

import System.Random

------------------------------------------------------------------------

data Seed = Seed StdGen

instance RandomGen Seed where
  split (Seed g) = let (g', g'') = split g in (Seed g', Seed g'')
  next  (Seed g) = fmap Seed (next g)

makeSeed :: Int -> Seed
makeSeed = Seed . mkStdGen

makeSeedIO :: IO Seed
makeSeedIO = fmap makeSeed randomIO

interval :: Seed -> (Double, Seed)
interval = randomR (0, 1)

list :: Seed -> Int -> (Seed -> (a, Seed)) -> ([a], Seed)
list s0 n0 g = go [] s0 n0
  where
    go acc s 0 = (reverse acc, s)
    go acc s n =
      let
        (x, s') = g s
      in
        go (x : acc) s' (n - 1)
