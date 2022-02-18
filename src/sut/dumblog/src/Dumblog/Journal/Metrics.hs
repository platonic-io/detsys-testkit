module Dumblog.Journal.Metrics where

import Journal.Internal.Metrics (MetricsSchema, Metrics) -- should maybe be moved to separate package
import qualified Journal.Internal.Metrics as Metrics

data DumblogCounters
  = CurrentNumberTransactions
  | NumberOfWrites
  | NumberOfReads
  | ErrorsEncountered
  deriving (Eq, Show, Enum, Bounded)

data DumblogHistograms
  = ResponseTime
  deriving (Eq, Show, Enum, Bounded)

type DumblogMetrics = Metrics DumblogCounters DumblogHistograms

dumblogSchema :: MetricsSchema DumblogCounters DumblogHistograms
dumblogSchema = Metrics.MetricsSchema 1
