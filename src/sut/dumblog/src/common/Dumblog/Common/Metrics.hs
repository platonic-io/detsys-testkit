module Dumblog.Common.Metrics where

import Data.Int (Int64)
import Data.Time (UTCTime, getCurrentTime, nominalDiffTimeToSeconds)
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)

import Journal.Internal.Metrics (Metrics, MetricsSchema)
import qualified Journal.Internal.Metrics as Metrics

------------------------------------------------------------------------

dumblogMetricsPath :: Int -> FilePath
dumblogMetricsPath port = "/dumblog-" ++ show port ++ ".metrics"

------------------------------------------------------------------------

data DumblogCounters
  = CurrentNumberTransactions
  | QueueDepth
  | ErrorsEncountered
  deriving (Eq, Show, Enum, Bounded)

data DumblogHistograms
  = Latency
  | ServiceTimeReads
  | ServiceTimeWrites
  | ResponseTime
  | WriteSize
  deriving (Eq, Show, Enum, Bounded)

type DumblogMetrics = Metrics DumblogCounters DumblogHistograms

dumblogSchema :: MetricsSchema DumblogCounters DumblogHistograms
dumblogSchema = Metrics.MetricsSchema 1

------------------------------------------------------------------------

nanosSinceEpoch :: UTCTime -> Int64
nanosSinceEpoch =
  floor . (1e9 *) . nominalDiffTimeToSeconds . utcTimeToPOSIXSeconds

getCurrentNanosSinceEpoch :: IO Int64
getCurrentNanosSinceEpoch = do
  now <- getCurrentTime
  return (nanosSinceEpoch now)
