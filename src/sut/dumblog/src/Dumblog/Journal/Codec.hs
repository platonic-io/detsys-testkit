{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}

module Dumblog.Journal.Codec where

import Data.Binary (Binary)
import qualified Data.Binary as Binary
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as LBS
import Data.Int (Int64)
import Data.Time (UTCTime, getCurrentTime, nominalDiffTimeToSeconds)
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import GHC.Generics (Generic)

------------------------------------------------------------------------

data Envelope a = Envelope
  { eLength :: !Int
  , eContent :: !a
  , eArrival :: !Int64 -- Nano seconds since epoch.
  } deriving stock (Functor, Generic)

instance Binary a => Binary (Envelope a) where

encode :: Binary a => Envelope a -> ByteString
encode e = LBS.toStrict (Binary.encode e)

-- This is guaranteed not to copy the bytestring but we should probably
-- allow this to fail.
decode :: Binary a => ByteString -> Envelope a
decode bs = Binary.decode $ LBS.fromStrict bs

nanosSinceEpoch :: UTCTime -> Int64
nanosSinceEpoch =
  floor . (1e9 *) . nominalDiffTimeToSeconds . utcTimeToPOSIXSeconds

getCurrentNanosSinceEpoch :: IO Int64
getCurrentNanosSinceEpoch = do
  now <- getCurrentTime
  return (nanosSinceEpoch now)
