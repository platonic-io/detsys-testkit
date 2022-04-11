{-# LANGUAGE DerivingStrategies #-}

module ATMC.Lec5.Time where

import Data.IORef
import Data.Time
import Data.Time.Clock
import Data.Time.Calendar.OrdinalDate

------------------------------------------------------------------------

newtype Time = Time UTCTime
  deriving stock (Eq, Ord, Show)

addTime :: NominalDiffTime -> Time -> Time
addTime secs (Time t) = Time (addUTCTime secs t)

data Clock = Clock
  { cGetCurrentTime :: IO Time
  , cSetCurrentTime :: Time -> IO ()
  }

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

fakeClockEpoch :: IO Clock
fakeClockEpoch = fakeClock epoch
