module Ldfi.Estimate where

import Data.List (subsequences)

------------------------------------------------------------------------

factorial :: Int -> Int
factorial n = product [1..n]

choose :: Int -> Int -> Int
choose n k = factorial n `div` (factorial k * factorial (n - k))

possibleCrashFaults :: Int -> Int -> Int
possibleCrashFaults nodes maxCrashes =
  sum [ nodes `choose` c  | c <- [0..maxCrashes] ]
-- ^ NOTE: The above doesn't factor in WHEN the crashes happen?!

possibleOmissionFaults :: Int -> Int
possibleOmissionFaults internalNetworkTraceLength = 2 ^ internalNetworkTraceLength
-- Or: length (subsequences [1..internalHistoryLength])

-- NOTE: Some of these orderings will break the happens-before relation, so
-- this should be seen as a pessimistic upper bound.
possibleOrderings :: Int -> Int
possibleOrderings internalNetworkTraceLength = factorial internalNetworkTraceLength

-- Combination of possible omissions and orderings.
possibleSchedules :: Int -> Int
possibleSchedules internalNetworkTraceLength =
  sum [ possibleOrderings (length s)
      | s <- subsequences [1..internalNetworkTraceLength]
      ]

-- Combination of omissions, orderings and crashes. Note that this doesn't not
-- take _new_ messages created during execution into account, e.g. from retries.
possibleFaults :: Int
possibleFaults = undefined

{-
  * if the receiver's apply function is commutative then orderings don't matter
  * if sender retries often enough then omissions don't matter
  * if receiver's apply function is idempotent retries are safe
  * if all above then there's only one outcome! Hard to test?
-}
