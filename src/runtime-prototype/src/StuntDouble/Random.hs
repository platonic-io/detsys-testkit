module StuntDouble.Random where

import System.Random

------------------------------------------------------------------------

data Seed = Seed StdGen

instance RandomGen Seed where
  split (Seed g) = let (g', g'') = split g in (Seed g', Seed g'')
  next  (Seed g) = fmap Seed (next g)

makeSeed :: Int -> Seed
makeSeed = Seed . mkStdGen

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

shuffle :: Seed -> [a] -> ([a], Seed)
shuffle s0 xs =
  let
    l = length xs - 1
    (rs, s') = list s0 l (randomR (0, l))
  in
    (shuffle1 xs rs, s')
  where
    -- The following code is taken from:
    -- http://okmij.org/ftp/Haskell/perfect-shuffle.txt
    shuffle1 :: [b] -> [Int] -> [b]
    shuffle1 [e] [] = [e]
    shuffle1 elements (r:r_others) =
      let (b,rest) = extract r elements
      in b:(shuffle1 rest r_others)
    shuffle1 _ _ = error "shuffle1: impossible"

    extract :: Int -> [a] -> (a, [a])
    extract 0  (h:t) = (h, t)
    extract j0 l     = loop j0 l []
      where
        loop 0 (h:t) accum = (h, accum ++ t)
        loop j (h:t) accum = loop (j-1) t (h:accum)
        loop _ _     _     = error "loop: impossible"
