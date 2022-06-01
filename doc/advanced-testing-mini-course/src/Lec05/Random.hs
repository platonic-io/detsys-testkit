module Lec05.Random where

import Data.IORef
import System.Random (StdGen, setStdGen, getStdGen, mkStdGen, randomR, randomIO)
import qualified System.Random

------------------------------------------------------------------------

data Random = Random
  { rGetStdGen :: IO StdGen
  , rSetStdGen :: StdGen -> IO ()
  }

newtype Seed = Seed { unSeed :: Int }

realRandom :: IO Random
realRandom =
  return Random
    { rGetStdGen = getStdGen
    , rSetStdGen = setStdGen
    }

fakeRandom :: Seed -> IO Random
fakeRandom (Seed seed) = do
  let g = mkStdGen seed
  r <- newIORef g
  return Random
    { rGetStdGen = readIORef r
    , rSetStdGen = writeIORef r
    }

randomInterval :: System.Random.Random a => Random -> (a, a) -> IO a
randomInterval random range = do
  g <- rGetStdGen random
  let (x, g') = randomR range g
  rSetStdGen random g'
  return x

generateSeeds :: Int -> IO [Seed]
generateSeeds nr = mapM (\_ -> fmap Seed randomIO) [1..nr]

data RandomDist
  = Uniform Double Double
  | Exponential Double

defaultRandomDist :: RandomDist
defaultRandomDist = Uniform 1 20

randomFromDist :: Random -> RandomDist -> IO Double
randomFromDist random (Uniform minV maxV) = do
  randomInterval random (minV, maxV)
randomFromDist random (Exponential lambda) = do
  -- https://en.wikipedia.org/wiki/Inverse_transform_sampling#Examples
  y <- randomInterval random (0.0, 1.0 :: Double)
  return $ -1/lambda*log (1 - y)
