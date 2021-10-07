{-# LANGUAGE DeriveGeneric #-}

module StuntDouble.Time where

import GHC.Generics (Generic)
import Data.Aeson (FromJSON, ToJSON)
import Control.Concurrent.STM
import qualified Data.Time as Time
import qualified Data.Time.Clock as Time
import qualified Data.Time.Calendar.OrdinalDate as Time

------------------------------------------------------------------------

newtype Time = Time Time.UTCTime
  deriving (Show, Read, Eq, Ord, Generic)

instance FromJSON Time
instance ToJSON Time

addTime :: Time -> Time.NominalDiffTime -> Time
addTime (Time t) dt = Time (Time.addUTCTime dt t)

afterTime :: Time -> Time -> Bool
afterTime (Time t1) (Time t2) = t1 >= t2

data Clock = Clock
  { getCurrentTime :: IO Time }

realClock :: Clock
realClock = Clock (Time <$> Time.getCurrentTime)

newtype FakeClockHandle = FakeClockHandle (TVar Time)

fakeClock :: Time -> IO (Clock, FakeClockHandle)
fakeClock t0 = do
  v <- newTVarIO t0
  return (Clock (readTVarIO v), FakeClockHandle v)

fakeClockEpoch :: IO (Clock, FakeClockHandle)
fakeClockEpoch = do
  let t0 = Time (Time.UTCTime (Time.fromOrdinalDate 1970 0) 0)
  v <- newTVarIO t0
  return (Clock (readTVarIO v), FakeClockHandle v)

advanceFakeClock :: FakeClockHandle -> Time.NominalDiffTime -> IO ()
advanceFakeClock (FakeClockHandle v) seconds =
  atomically (modifyTVar' v (flip addTime seconds))

-- XXX: move to test directory.
test :: IO ()
test = do
  t <- Time.getCurrentTime
  (clock, h) <- fakeClock (Time t)
  print =<< getCurrentTime clock
  print =<< getCurrentTime clock
  advanceFakeClock h 1
  print =<< getCurrentTime clock
