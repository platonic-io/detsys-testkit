module Lec03.ServiceTest where

import Control.Concurrent
import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Concurrent.STM.TQueue
import Control.Monad
import qualified Data.ByteString.Char8 as BS8
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Lazy.Char8 as LBS8
import Data.List (permutations)
import Data.Tree (Forest, Tree(Node))
import Data.Vector (Vector)
import qualified Data.Vector as Vector
import Network.HTTP.Client
       ( Manager
       , RequestBody(RequestBodyLBS)
       , defaultManagerSettings
       , httpLbs
       , method
       , newManager
       , parseRequest
       , path
       , requestBody
       , responseBody
       , responseStatus
       )
import Network.HTTP.Types (status200)
import System.Directory
import System.Random
import Test.HUnit hiding (assert, path)
import Test.QuickCheck
import Test.QuickCheck.Monadic
import Text.Read (readMaybe)

import Lec02ConcurrentSMTesting
       ( History'(History)
       , Operation'(Ok)
       , interleavings
       , linearisable
       , prettyHistory
       , toPid
       , classifyCommandsLength
       )
import Lec03.Service

------------------------------------------------------------------------

newtype Index = Index Int
  deriving (Eq, Show)

data ClientRequest = WriteReq ByteString | ReadReq Index
  deriving Show

data ClientResponse = WriteResp Index | ReadResp (Maybe ByteString)
  deriving (Eq, Show)

newtype ConcProgram = ConcProgram { unConcProgram :: [[ClientRequest]] }
  deriving Show

newtype Model = Model (Vector ByteString)

initModel :: Model
initModel = Model Vector.empty

step :: Model -> ClientRequest -> (Model, ClientResponse)
step (Model vec) (WriteReq bs) =
  (Model (Vector.snoc vec bs), WriteResp (Index (Vector.length vec)))
step (Model vec) (ReadReq (Index ix)) =
  (Model vec, ReadResp (vec Vector.!? ix))

type Operation = Operation' ClientRequest ClientResponse

concExec :: Manager -> TQueue Operation -> ClientRequest -> IO ()
concExec mgr hist req = do
  initReq <- parseRequest ("http://localhost:" ++ show pORT)
  case req of
    WriteReq bs -> do
      resp <- httpLbs initReq { method = "POST"
                              , requestBody = RequestBodyLBS bs
                              } mgr
      pid <- toPid <$> myThreadId
      atomically (writeTQueue hist
                  (Ok pid (WriteResp (Index (read (LBS8.unpack (responseBody resp)))))))
    ReadReq (Index ix) -> do
      resp <- httpLbs initReq { method = "GET"
                              , path = path initReq <> BS8.pack (show ix)
                              } mgr
      pid <- toPid <$> myThreadId
      if responseStatus resp == status200
      then atomically (writeTQueue hist (Ok pid (ReadResp (Just (responseBody resp)))))
      else atomically (writeTQueue hist (Ok pid (ReadResp Nothing)))

genClientRequest :: Gen ClientRequest
genClientRequest = oneof
  [ WriteReq <$> (LBS.pack <$> arbitrary)
  , ReadReq <$> (Index <$> arbitrary)
  ]

validProgram :: Model -> [ClientRequest] -> Bool
validProgram _model _cmds = True

shrinkClientRequest :: ClientRequest -> [ClientRequest]
shrinkClientRequest (WriteReq bs) = [ WriteReq (LBS.pack s') | s' <- shrink (LBS.unpack bs) ]
shrinkClientRequest (ReadReq _ix) = []

genConcProgram :: Model -> Gen ConcProgram
genConcProgram m0 = sized (go m0 [])
  where
    go :: Model -> [[ClientRequest]] -> Int -> Gen ConcProgram
    go m acc sz | sz <= 0   = return (ConcProgram (reverse acc))
                | otherwise = do
                    n <- chooseInt (2, 5)
                    reqs <- vectorOf n genClientRequest `suchThat` concSafe m
                    go (advanceModel m reqs) (reqs : acc) (sz - n)

advanceModel :: Model -> [ClientRequest] -> Model
advanceModel m reqs = foldl (\ih req -> fst (step ih req)) m reqs

concSafe :: Model -> [ClientRequest] -> Bool
concSafe m = all (validProgram m) . permutations

validConcProgram :: Model -> ConcProgram -> Bool
validConcProgram m0 (ConcProgram reqss0) = go m0 True reqss0
  where
    go :: Model -> Bool -> [[ClientRequest]] -> Bool
    go m False _              = False
    go m acc   []             = acc
    go m acc   (reqs : reqss) = go (advanceModel m reqs) (concSafe m reqs) reqss

shrinkConcProgram :: Model -> ConcProgram -> [ConcProgram]
shrinkConcProgram m
  = filter (validConcProgram m)
  . map ConcProgram
  . filter (not . null)
  . shrinkList (shrinkList shrinkClientRequest)
  . unConcProgram

prettyConcProgram :: ConcProgram -> String
prettyConcProgram = show

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
  asyncService queue

stopService :: Async () -> IO ()
stopService pid = cancel pid

prop_collaborationTests :: Property
prop_collaborationTests = mapSize (min 20) $
  forAllConcProgram $ \(ConcProgram reqss) -> monadicIO $ do
    monitor (classifyCommandsLength (concat reqss))
    monitor (tabulate "Client requests" (map constructorString (concat reqss)))
    monitor (tabulate "Number of concurrent client requests" (map (show . length) reqss))
    -- Rerun a couple of times, to avoid being lucky with the interleavings.
    mgr <- run (newManager defaultManagerSettings)
    replicateM_ 10 $ do
      pid <- run startService
      queue <- run newTQueueIO
      run (mapM_ (mapConcurrently (concExec mgr queue)) reqss)
      run (stopService pid)
      hist <- History <$> run (atomically (flushTQueue queue))
      assertWithFail (linearisable step initModel (interleavings hist)) (prettyHistory hist)
  where
    constructorString :: ClientRequest -> String
    constructorString WriteReq {} = "WriteReq"
    constructorString ReadReq  {} = "ReadReq"

    assertWithFail :: Monad m => Bool -> String -> PropertyM m ()
    assertWithFail condition msg = do
      unless condition $
        monitor (counterexample ("Failed: " ++ msg))
      assert condition
