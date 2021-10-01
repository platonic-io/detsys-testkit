module StuntDouble.Time where

import Control.Concurrent.STM
import qualified Data.Time as Time
import qualified Data.Time.Calendar.OrdinalDate as Time

------------------------------------------------------------------------

type Timestamp = Time.UTCTime

data Time = Time
  { getCurrentTime :: IO Timestamp }

realTime :: Time
realTime = Time Time.getCurrentTime

newtype FakeTimeHandle = FakeTimeHandle (TVar Time.UTCTime)

fakeTime :: Time.UTCTime -> IO (Time, FakeTimeHandle)
fakeTime t0 = do
  v <- newTVarIO t0
  return (Time (readTVarIO v), FakeTimeHandle v)

fakeTimeEpoch :: IO (Time, FakeTimeHandle)
fakeTimeEpoch = do
  let t0 = Time.UTCTime (Time.fromOrdinalDate 1970 0) 0
  v <- newTVarIO t0
  return (Time (readTVarIO v), FakeTimeHandle v)

advanceFakeTime :: FakeTimeHandle -> Time.NominalDiffTime -> IO ()
advanceFakeTime (FakeTimeHandle v) seconds =
  atomically (modifyTVar' v (Time.addUTCTime seconds))

-- XXX: move to test directory.
test :: IO ()
test = do
  t <- Time.getCurrentTime
  (time, h) <- fakeTime t
  print =<< getCurrentTime time
  print =<< getCurrentTime time
  advanceFakeTime h 1
  print =<< getCurrentTime time
