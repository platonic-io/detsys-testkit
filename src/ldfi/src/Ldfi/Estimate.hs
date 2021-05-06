{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module Ldfi.Estimate where

import Data.List (genericLength, subsequences)
import qualified Data.Set as Set
import Data.String (IsString)
import GHC.Exts (IsList (..))

------------------------------------------------------------------------

newtype Node = Node String
  deriving (Eq, Ord, Show, IsString)

newtype Nodes = Nodes [Node]

nodes1 :: Nodes
nodes1 = Nodes [Node "A"]

nodes2 :: Nodes
nodes2 = Nodes [Node "A", "B"]

nodes3 :: Nodes
nodes3 = Nodes [Node "A", "B", "C"]

nodes4 :: Nodes
nodes4 = Nodes [Node "A", "B", "C", "D"]

nodes5 :: Nodes
nodes5 = Nodes [Node "A", "B", "C", "D", "E"]

newtype Eff = Eff Integer
  deriving (Num)

newtype Eot = Eot Integer
  deriving (Num)

newtype MaxCrashes = MaxCrashes Integer
  deriving (Num)

newtype NetworkTraceF a = NetworkTrace [a]

instance IsList (NetworkTraceF a) where
  type Item (NetworkTraceF a) = a
  toList (NetworkTrace xs) = xs
  fromList xs = NetworkTrace xs

type NetworkTrace = NetworkTraceF Int

------------------------------------------------------------------------

-- | Factorial.
factorial :: (Num a, Enum a) => a -> a
factorial n = product (enumFromTo 1 n)

-- | The binomial coefficient.
choose :: Integral a => a -> a -> a
choose n k = factorial n `div` (factorial k * factorial (n - k))

-- | Cross product.
cross :: [a] -> [b] -> [(a, b)]
cross xs ys = [(x, y) | x <- xs, y <- ys]

-- | Powerset on lists.
powerSet :: Ord a => [a] -> [[a]]
powerSet = Set.toList . Set.map Set.toList . Set.powerSet . Set.fromList

prop_crossLength :: [a] -> Bool
prop_crossLength xs = length (cross xs xs) == length xs ^ (2 :: Integer)

------------------------------------------------------------------------

-- | Possible crash failures.
possibleCrashFaults :: Nodes -> MaxCrashes -> NetworkTrace -> [[(Node, Int)]]
possibleCrashFaults (Nodes nodes) (MaxCrashes maxCrashes) (NetworkTrace xs) =
  [ fs
    | fs <- powerSet (cross nodes xs),
      genericLength fs <= maxCrashes,
      Set.size (Set.fromList (map fst fs)) == length fs
  ]

possibleCrashFaultsCount :: Nodes -> MaxCrashes -> NetworkTrace -> Integer
possibleCrashFaultsCount (Nodes nodes) (MaxCrashes maxCrashes) (NetworkTrace xs) =
  -- We use `max 1` because no faults, i.e. `[]` should count as one possibility
  -- rather than zero.
  sum
    [ max 1 l
      | fs <- powerSet (cross nodes xs),
        let l = genericLength fs,
        l <= maxCrashes,
        Set.size (Set.fromList (map fst fs)) == fromEnum l
    ]

-- Or: sum (map (max 1 . genericLength) (possibleCrashFaults nodes maxCrashes networkTrace))
-- NOTE: This function is very slow and unusable for bigger inputs.

possibleOmissionFaults :: NetworkTrace -> Integer
possibleOmissionFaults (NetworkTrace xs) = 2 ^ length xs

-- Or: length (subsequences xs)

-- NOTE: Some of these orderings will break the happens-before relation, so
-- this should be seen as a pessimistic upper bound.
possibleOrderings :: NetworkTrace -> Integer
possibleOrderings (NetworkTrace xs) = factorial (genericLength xs)

-- Combination of possible omissions and orderings.
possibleSchedules :: NetworkTrace -> Integer
possibleSchedules (NetworkTrace xs) =
  sum
    [ possibleOrderings (NetworkTrace ys)
      | ys <- subsequences xs
    ]

-- Combination of omissions, orderings and crashes. NOTE that this doesn't not
-- take _new_ messages created during execution into account, e.g. from retries.
possibleFaults :: Nodes -> MaxCrashes -> NetworkTrace -> Integer
possibleFaults (Nodes nodes) (MaxCrashes maxCrashes) (NetworkTrace xs) =
  undefined

{-
  * if the receiver's apply function is commutative then orderings don't matter
  * if sender retries often enough then omissions don't matter
  * if receiver's apply function is idempotent retries are safe
  * if all above then there's only one outcome! Hard to test?
-}

------------------------------------------------------------------------

-- Estimates from the Molly implementation of LDFI:

-- https://github.com/palvaro/molly/blob/a3a6d7950814e1154253357a03e7d64754c464b8/src/main/scala/edu/berkeley/cs/boom/molly/FailureSpec.scala#L22

{-
    // We'll count the failure scenarios of each node, then multiply them to get
    // the total count.

    // At each time step before the crash, any of the `nodes.size - 1` channels could fail.
    val singleTimeLosses = BigInt(2).pow(nodes.size - 1)
    val crashFree = singleTimeLosses.pow(eff)

    // Crashed nodes can't send messages:
    val crashProne = (1 to eot + 1).map { crashTime => singleTimeLosses.pow(Math.min(eff, crashTime - 1)) }.sum
    // (ways to pick which nodes don't crash)   * (executions of crash-free nodes)        * (executions of crash prone nodes)
    binomialCoefficient(nodes.size, maxCrashes) * crashFree.pow(nodes.size - maxCrashes) * crashProne.pow(maxCrashes)
  }
-}

singleTimeLosses :: Nodes -> Integer
singleTimeLosses (Nodes nodes) = 2 ^ (length nodes - 1)

crashFree :: Nodes -> Eff -> Integer
crashFree nodes (Eff eff) = singleTimeLosses nodes ^ eff

crashProne :: Nodes -> Eot -> Eff -> Integer
crashProne nodes (Eot eot) (Eff eff) =
  sum $
    map (\crashTime -> singleTimeLosses nodes ^ (min eff (crashTime - 1))) [1 .. eot + 1]

grossEstimate :: Eot -> Eff -> MaxCrashes -> Nodes -> Integer
grossEstimate eot eff (MaxCrashes maxCrashes) nodes@(Nodes nodes_) =
  (genericLength nodes_ `choose` maxCrashes)
    * ((crashFree nodes eff) ^ (genericLength nodes_ - maxCrashes))
    * ((crashProne nodes eot eff) ^ maxCrashes)
