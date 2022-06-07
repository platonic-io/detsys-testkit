{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE NumericUnderscores #-}

module Lec05.Time
  ( module Lec05.Time
  , NominalDiffTime ) where

import Data.Int (Int64)
import Data.IORef
import Data.Time
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import Data.Time.Calendar.OrdinalDate

------------------------------------------------------------------------

newtype Time = Time UTCTime
  deriving stock (Eq, Ord, Show)

addTimeSeconds :: NominalDiffTime -> Time -> Time
addTimeSeconds secs (Time t) = Time (addUTCTime secs t)

addTimeMicros :: Int -> Time -> Time
addTimeMicros micros (Time t) = Time (addUTCTime (realToFrac micros * 0.000001) t)

diffTimeMicros :: Time -> Time -> Int
diffTimeMicros (Time t0) (Time t1) =
  round (realToFrac (fromEnum (diffUTCTime t0 t1)) * 0.000001)
  -- The `NominalTimeDiff` we get from `diffUTCTime` has a precision of one
  -- picosecond (= 10^-12 s), and according to the documentation " Enumeration
  -- functions will treat it as picoseconds.", so hence the `fromEnum` and `*
  -- 10^-6`.

data Clock = Clock
  { cGetCurrentTime :: IO Time
  , cSetCurrentTime :: Time -> IO ()
  }

cModifyCurrentTime :: Clock -> (Time -> Time) -> IO ()
cModifyCurrentTime clock f = do
  t <- cGetCurrentTime clock
  cSetCurrentTime clock (f t)

realClock :: IO Clock
realClock = return Clock
  { cGetCurrentTime = Time <$> getCurrentTime
  , cSetCurrentTime = \_ -> return () -- Can't change the real clock.
  }

fakeClock :: Time -> IO Clock
fakeClock t0 = do
  ref <- newIORef t0
  return Clock
    { cGetCurrentTime = readIORef ref
    , cSetCurrentTime = writeIORef ref
    }

epoch :: Time
epoch = Time (UTCTime (fromOrdinalDate 1970 0) 0)

fromEpoch :: Time -> Int64
fromEpoch (Time t) = truncate
  . (*1_000_000_000)
  . toRational
  $ utcTimeToPOSIXSeconds t

fakeClockEpoch :: IO Clock
fakeClockEpoch = fakeClock epoch
