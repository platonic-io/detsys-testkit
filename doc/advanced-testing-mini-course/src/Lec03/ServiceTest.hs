module Lec03.ServiceTest where

import Control.Concurrent
import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Concurrent.STM.TQueue
import Control.Monad
import Data.ByteString (ByteString)
import Data.Vector (Vector)
import qualified Data.Vector as Vector
import Data.List (permutations)
import Data.Tree (Forest, Tree(Node))
import System.Directory
import System.Random
import Test.HUnit hiding (assert)
import Test.QuickCheck
import Test.QuickCheck.Monadic

import Lec02ConcurrentSMTesting
       ( History'(History)
       , Operation'
       , interleavings
       , linearisable
       , prettyHistory
       )
import Lec03.Service

------------------------------------------------------------------------

newtype Index = Index Int
  deriving (Eq, Show)

data ClientRequest = WriteReq ByteString | ReadReq Index
  deriving Show

data ClientResponse = WriteResp Index | ReadResp (Maybe ByteString)
  deriving (Eq, Show)

newtype ConcProgram = ConcProgram [[ClientRequest]]

newtype Model = Model (Vector ByteString)

initModel :: Model
initModel = Model Vector.empty

step :: Model -> ClientRequest -> (Model, ClientResponse)
step (Model vec) (WriteReq bs) =
  (Model (Vector.snoc vec bs), WriteResp (Index (Vector.length vec + 1)))
step (Model vec) (ReadReq (Index ix)) =
  (Model vec, ReadResp  (vec Vector.!? ix))

type Operation = Operation' ClientRequest ClientResponse

concExec :: TQueue Operation -> ClientRequest -> IO ()
concExec = undefined

genConcProgram :: Model -> Gen ConcProgram
genConcProgram = undefined

shrinkConcProgram :: Model -> ConcProgram -> [ConcProgram]
shrinkConcProgram = undefined

prettyConcProgram :: ConcProgram -> String
prettyConcProgram = undefined

forAllConcProgram :: (ConcProgram -> Property) -> Property
forAllConcProgram k =
  forAllShrinkShow (genConcProgram m) (shrinkConcProgram m) prettyConcProgram k
  where
    m = initModel

startService :: IO (Async ())
startService = do
  removePathForcibly sQLITE_DB_PATH
  -- NOTE: fake queue is used here, justified by previous contract testing.
  queue <- fakeQueue mAX_QUEUE_SIZE
  async (service queue)

stopService :: Async () -> IO ()
stopService pid = cancel pid

prop_concurrent :: Property
prop_concurrent = mapSize (min 20) $
  forAllConcProgram $ \(ConcProgram reqss) -> monadicIO $ do
    monitor (classifyCommandsLength (concat reqss))
    monitor (tabulate "Client requests" (map constructorString (concat reqss)))
    monitor (tabulate "Number of concurrent client requests" (map (show . length) reqss))
    -- Rerun a couple of times, to avoid being lucky with the interleavings.
    replicateM_ 10 $ do
      pid <- run startService
      queue <- run newTQueueIO
      run (mapM_ (mapConcurrently (concExec queue)) reqss)
      run (stopService pid)
      hist <- History <$> run (atomically (flushTQueue queue))
      assertWithFail (linearisable step initModel (interleavings hist)) (prettyHistory hist)
  where
    classifyCommandsLength :: [ClientRequest] -> Property -> Property
    classifyCommandsLength reqs
      = classify (length reqs == 0)                        "length requests: 0"
      . classify (0   < length reqs && length reqs <= 10)  "length requests: 1-10"
      . classify (10  < length reqs && length reqs <= 50)  "length requests: 11-50"
      . classify (50  < length reqs && length reqs <= 100) "length requests: 51-100"
      . classify (100 < length reqs && length reqs <= 200) "length requests: 101-200"
      . classify (200 < length reqs && length reqs <= 500) "length requests: 201-500"
      . classify (500 < length reqs)                       "length requests: >501"

    constructorString :: ClientRequest -> String
    constructorString WriteReq {} = "WriteReq"
    constructorString ReadReq  {} = "ReadReq"

    assertWithFail :: Monad m => Bool -> String -> PropertyM m ()
    assertWithFail condition msg = do
      unless condition $
        monitor (counterexample ("Failed: " ++ msg))
      assert condition
