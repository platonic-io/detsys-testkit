{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
module Ltl.Storage where

import Database.SQLite.Simple
import qualified Data.Aeson as Aeson
import Data.ByteString.Lazy (ByteString)
import qualified Data.HashMap.Strict as HashMap
import Data.List.NonEmpty (NonEmpty(..))
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as Text
import qualified Data.Text.Lazy.Encoding as TextEncoding

import Control.Exception (throwIO)
import System.Environment (getEnv)
import System.FilePath ((</>))
import System.IO.Error (catchIOError, isDoesNotExistError)

import Ltl.Json
import Ltl.Traces

type TestId = Int
type RunId = Int

data Storage m = Storage
  { load :: TestId -> RunId -> m Trace
  }

  {-
-- This could Functor1 or FunctorB if we would import those
changeEffect :: (forall a. m a -> n a) -> Storage m -> Storage n
changeEffect act (Storage l) = Storage ((act .) . l)
-}

getDbPath :: IO String
getDbPath = do
  getEnv "DETSYS_DB"
    `catchIOError` \(e :: catchIOError) ->
      if isDoesNotExistError e
        then do
          home <- getEnv "HOME"
          return (home </> ".detsys.db")
        else throwIO e

data ExecutionStep = ExecutionStep
  { esReactor :: String,
    esLogicalTime :: Int,
    esHeapDiff :: Text,
    esMessage :: Text,
    esEvent :: Text
  }

instance FromRow ExecutionStep where
  fromRow = ExecutionStep <$> field <*> field <*> field <*> field <*> field

sqliteLoadExecutionSteps:: TestId -> RunId -> IO [ExecutionStep]
sqliteLoadExecutionSteps testId runId = do
  path <- getDbPath
  conn <- open path
  queryNamed conn
    "SELECT execution_step.reactor, \
    \       execution_step.logical_time, \
    \       execution_step.heap_diff, \
    \       network_trace.message, \
    \       network_trace.args \
    \FROM execution_step \
    \INNER JOIN network_trace \
    \ON (execution_step.test_id=network_trace.test_id \
    \   AND execution_step.run_id = network_trace.run_id \
    \   AND execution_step.logical_time=network_trace.recv_logical_time) \
    \WHERE \
    \  execution_step.test_id=:testId \
    \  AND execution_step.run_id=:runId \
    \ORDER BY execution_step.logical_time ASC"
    [":testId" := testId, ":runId" := runId]

data DeploymentInfo = DeploymentInfo
  { diReactor :: String,
    diState :: Json
  } deriving Show

instance Aeson.FromJSON DeploymentInfo where
  parseJSON = Aeson.withObject "Deployment" $ \v -> DeploymentInfo
    <$> v Aeson..: "reactor"
    <*> v Aeson..: "args"

sqliteLoadInitDeployment :: TestId -> IO (Map Node Json)
sqliteLoadInitDeployment testId = do
  path <- getDbPath
  conn <- open path
  r <- queryNamed conn
       "SELECT deployment FROM test_info \
       \ WHERE test_id = :testId"
       [":testId" := testId] ::
    IO [Only Text]
  case r of
    [Only t] -> case Aeson.decode (TextEncoding.encodeUtf8 t) of
      Nothing -> error $ "test " ++ show testId ++ " has invalid deployment (can't decode): " ++ show t
      Just ds -> return $ Map.fromList [(diReactor d, diState d) | d <- ds]
    _ -> error $ "not unique test info for test " ++ show testId ++ ": " ++ show r

mkTrace :: [ExecutionStep] -> Map Node Json -> Trace
mkTrace es s = case go es s of
  [] -> error "empty execution steps"
  (x:xs) -> x :| xs
  where
    updateState es = Map.adjust (\v -> mergePatch v (Maybe.fromMaybe ("Can't decode heap") $ Aeson.decode $ TextEncoding.encodeUtf8 $ esHeapDiff es)) (esReactor es)
    go [] _ = []
    go (es:ess) state =
      let
        state' = updateState es state
        sb = StateBehaviour
          { before = State state
          , worldTime = esLogicalTime es
          , action = Event $ Aeson.Object (HashMap.fromList [
                                              ("message", Aeson.String $ Text.toStrict $ esMessage es),
                                              ("event", Maybe.fromMaybe "(Can't decode event)" $ Aeson.decode $ TextEncoding.encodeUtf8 $ esEvent es)])
          , after = State state'
          }
      in sb : go ess state'

sqliteLoad :: TestId -> RunId -> IO Trace
sqliteLoad testId runId = do
  es <- sqliteLoadExecutionSteps testId runId
  init <- sqliteLoadInitDeployment testId
  pure $ mkTrace es init

sqliteStorage :: Storage IO
sqliteStorage = Storage { load = sqliteLoad }
