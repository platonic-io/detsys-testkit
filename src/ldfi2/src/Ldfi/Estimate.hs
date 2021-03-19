module Ldfi.Estimate where

import Data.List (genericLength, subsequences)

------------------------------------------------------------------------

factorial :: (Num a, Enum a) => a -> a
factorial n = product [1..n]

-- The binomial coefficient.
choose :: Integral a => a -> a -> a
choose n k = factorial n `div` (factorial k * factorial (n - k))

possibleCrashFaults :: Integer -> Integer -> Integer
possibleCrashFaults nodes maxCrashes =
  sum [ nodes `choose` c  | c <- [0..maxCrashes] ]
-- ^ NOTE: The above doesn't factor in WHEN the crashes happen?!

possibleOmissionFaults :: Integer -> Integer
possibleOmissionFaults internalNetworkTraceLength = 2 ^ internalNetworkTraceLength
-- Or: length (subsequences [1..internalHistoryLength])

-- NOTE: Some of these orderings will break the happens-before relation, so
-- this should be seen as a pessimistic upper bound.
possibleOrderings :: Integer -> Integer
possibleOrderings internalNetworkTraceLength = factorial internalNetworkTraceLength

-- Combination of possible omissions and orderings.
possibleSchedules :: Integer -> Integer
possibleSchedules internalNetworkTraceLength =
  sum [ possibleOrderings (genericLength s)
      | s <- subsequences [1..internalNetworkTraceLength]
      ]

-- Combination of omissions, orderings and crashes. NOTE that this doesn't not
-- take _new_ messages created during execution into account, e.g. from retries.
possibleFaults :: Integer
possibleFaults = undefined

{-
  * if the receiver's apply function is commutative then orderings don't matter
  * if sender retries often enough then omissions don't matter
  * if receiver's apply function is idempotent retries are safe
  * if all above then there's only one outcome! Hard to test?
-}

------------------------------------------------------------------------

-- Estimates from the Molly implementation of LDFI:

-- https://github.com/palvaro/molly/blob/a3a6d7950814e1154253357a03e7d64754c464b8/src/main/scala/edu/berkeley/cs/boom/molly/FailureSpec.scala#L22
-- https://github.com/palvaro/molly/blob/a3a6d7950814e1154253357a03e7d64754c464b8/src/test/scala/edu/berkeley/cs/boom/molly/FailureSpecSuite.scala#L6

{-
    // We'll count the failure scenarios of each node, then multiply them to get the total count.
    // At each time step before the crash, any of the `nodes.size - 1` channels could fail.
    val singleTimeLosses = BigInt(2).pow(nodes.size - 1)
    val crashFree = singleTimeLosses.pow(eff)
    // Crashed nodes can't send messages:
    val crashProne = (1 to eot + 1).map { crashTime => singleTimeLosses.pow(Math.min(eff, crashTime - 1)) }.sum
    // (ways to pick which nodes don't crash)   * (executions of crash-free nodes)        * (executions of crash prone nodes)
    binomialCoefficient(nodes.size, maxCrashes) * crashFree.pow(nodes.size - maxCrashes) * crashProne.pow(maxCrashes)
  }
-}

singleTimeLosses :: Integer -> Integer
singleTimeLosses nodes = 2 ^ (nodes - 1)

crashFree :: Integer -> Integer -> Integer
crashFree nodes eff = 2 ^ (singleTimeLosses nodes ^ eff)

crashProne :: Integer -> Integer -> Integer -> Integer
crashProne nodes eff eot  = sum $
  map (\crashTime -> singleTimeLosses nodes ^ (min eff (crashTime - 1))) [1..eot + 1]

grossEstimate :: Integer -> Integer -> Integer -> Integer -> Integer
grossEstimate nodes eff eot maxCrashes =
  nodes `choose` maxCrashes *
  crashFree nodes eff ^ (nodes - maxCrashes) *
  crashProne nodes eff eot ^ maxCrashes
