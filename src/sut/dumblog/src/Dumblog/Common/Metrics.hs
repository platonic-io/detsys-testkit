module Dumblog.Common.Metrics where

import Journal.Internal.Metrics (Metrics, MetricsSchema)
import qualified Journal.Internal.Metrics as Metrics

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
