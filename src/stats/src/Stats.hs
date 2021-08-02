module Stats where

import Control.Monad (when)
import Data.List (transpose)
import qualified Data.Time.Clock as Time
import Description (Description (items), Item (formula), Result)
import Histogram (Histogram)
import qualified Histogram
import Ltl (ltl)
import qualified Storage

findRunIds :: Int -> IO [Int]
findRunIds testId = do
  Storage.sqliteGetRuns testId

timeIt :: Histogram -> IO a -> IO a
timeIt histogram action = do
  startTime <- Time.getCurrentTime
  r <- action
  endTime <- Time.getCurrentTime
  let delta = (* 1000) . Time.nominalDiffTimeToSeconds $ Time.diffUTCTime endTime startTime
  Histogram.measure delta histogram
  pure r

-- checkItem :: Histogram -> Int -> Int -> String -> IO Bool
-- checkItem histogram testId runId formula = do
--   result <- timeIt histogram $ ltl testId runId formula
--   pure result

checkRun :: Histogram -> Int -> Int -> [Item String] -> IO [Item Bool]
checkRun histogram testId runId items = do
  let formulas = map formula items
  results <- timeIt histogram $ ltl testId runId formulas
  when (length items /= length results) $
    error $ "Can't match number of results returned by ltl:\n " ++ show results
  pure $ zipWith (\i r -> i {formula = r}) items results

mySequence :: [Item a] -> Item [a]
mySequence [] = error "No items"
mySequence xs@(i : _) = i {formula = map formula xs}

gatherInformation :: Int -> Description String -> IO (Result, Int)
gatherInformation testId desc = do
  runIds <- findRunIds testId
  histogram <- Histogram.newHistogram
  results <- mapM (\rId -> checkRun histogram testId rId (items desc)) runIds
  Histogram.prettyPrintHistogram "LTL Timings (in ms)" histogram
  putStrLn ""
  pure
    ( desc
        { items = map (fmap sum . mySequence) . transpose . map (map (fmap bool2Int)) $ results
        },
      length runIds
    )
  where
    bool2Int False = 0
    bool2Int True = 1
