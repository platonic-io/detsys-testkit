module Journal.Internal.MetricTest where

import Control.Monad (forM_, unless)
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import System.Directory
       (canonicalizePath, getTemporaryDirectory, removePathForcibly)
import System.IO (hClose, openTempFile)
import Test.QuickCheck
import Test.QuickCheck.Monadic

import Journal.Internal.Metrics
import Journal.Internal.Utils hiding (assert)

-- can we generate this type?
data Counter = CA | CB | CC
  deriving (Enum, Bounded, Show, Eq, Ord)

data Histogram = Histogram
  deriving (Enum, Bounded)

schema :: MetricsSchema Counter Histogram
schema = MetricsSchema 1

genCounter :: Gen Counter
genCounter = elements [minBound .. maxBound]

data Command
  = IncrCounter Counter Int
  | ReadCounter Counter
  deriving (Show, Eq)

constructorString :: Command -> String
constructorString (IncrCounter c _) = "IncrCounter " <> show c
constructorString c@(ReadCounter{}) = show c

data Model = Model
  { mCounters :: Map Counter Int
  } deriving Show

newModel :: Model
newModel = Model
  { mCounters = Map.fromList [(c, 0) | c <- [minBound .. maxBound]]}

precondition :: Model -> Command -> Bool
precondition _ _ = True

data Response = Unit () | Int Int
  deriving (Eq, Show)

prettyResponse :: Response -> String
prettyResponse = show

step :: Command -> Model -> (Model, Response)
step (IncrCounter c v) m = ( m { mCounters = Map.update (Just . (+ v)) c (mCounters m)}, Unit ())
step (ReadCounter c) m = (m, Int $ Maybe.fromJust $ Map.lookup c (mCounters m))

validProgram :: Model -> [Command] -> Bool
validProgram = go True
  where
    go False _m _cmds       = False
    go valid _m []          = valid
    go valid m (cmd : cmds) = go (precondition m cmd) (fst (step cmd m)) cmds

genCommand :: Model -> Gen Command
genCommand _ = frequency
  [ (,) 1 $ IncrCounter <$> genCounter <*> arbitrary
  , (,) 1 $ ReadCounter <$> genCounter
  ]

genCommands :: Model -> Gen [Command]
genCommands m0 = sized (go m0)
  where
    go :: Model -> Int -> Gen [Command]
    go _m 0 = return []
    go m  n = do
      cmd <- genCommand m `suchThat` precondition m
      cmds <- go (fst (step cmd m)) (n - 1)
      return (cmd : cmds)

shrinkCommand :: Model -> Command -> [Command]
shrinkCommand _ _ = []

shrinkProgram :: Model -> [Command] -> [[Command]]
shrinkProgram model = filter (validProgram model) . shrinkList (shrinkCommand model)

exec :: Command -> Metrics Counter Histogram -> IO Response
exec (IncrCounter c v) metrics =
  Unit <$> incrCounter metrics c v
exec (ReadCounter c) metrics =
  Int <$> getCounter metrics c


-- we should maybe generalise this Property code, it similar to ByteBufferTest and JournalTest
prop_counter :: Property
prop_counter = do
  let model = newModel
  forAllShrink (genCommands model) (shrinkProgram model) $ \cmds -> monadicIO $ do
    run (putStrLn ("Generated commands: " <> show cmds))
    tmp <- run (canonicalizePath =<< getTemporaryDirectory)
    (fp, h) <- run (openTempFile tmp "MetricsTest")
    run (print fp)
    run (fallocate fp $ metricSize schema)
    metrics <- run (newMetrics schema fp)
    run (hClose h)
    monitor (tabulate "Commands" (map constructorString cmds))
    go model metrics cmds
  where
    go _ _ [] = return True
    go model metrics (cmd : cmds) = do
      let (model', resp) = step cmd model
      resp' <- run $ exec cmd metrics
      assertWithFail (resp == resp') $
        "expected: " ++ prettyResponse resp ++ "\n        got: " ++ prettyResponse resp'
      go model' metrics cmds

    assertWithFail :: Monad m => Bool -> String -> PropertyM m ()
    assertWithFail condition msg = do
      unless condition $
        monitor (counterexample ("Failed, " ++ msg))
      assert condition
