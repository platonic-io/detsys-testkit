module ATMC.Lec5.Random where

import Data.IORef
import System.Random (StdGen, setStdGen, getStdGen, mkStdGen)

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
