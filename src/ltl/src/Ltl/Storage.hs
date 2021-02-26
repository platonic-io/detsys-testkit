{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
module Ltl.Storage where

-- import Data.Aeson (FromJSON(..))
import qualified Data.Aeson as Aeson
import Data.ByteString.Lazy (ByteString)
import Database.SQLite.Simple
import Data.List.NonEmpty (NonEmpty(..))
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import Data.Text.Lazy (Text)
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
    esHeapDiff :: Text
  }

instance FromRow ExecutionStep where
  fromRow = ExecutionStep <$> field <*> field <*> field

sqliteLoadExecutionSteps:: TestId -> RunId -> IO [ExecutionStep]
sqliteLoadExecutionSteps testId runId = do
  path <- getDbPath
  conn <- open path
  queryNamed conn
       "SELECT reactor, logical_time, heap_diff FROM execution_step \
       \ WHERE test_id = :testId \
       \ AND run_id = :runId \
       \ ORDER BY logical_time ASC"
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
    go (es:ess) state = let state' = updateState es state in StateBehaviour (State state) Event (State state') : go ess state'

sqliteLoad :: TestId -> RunId -> IO Trace
sqliteLoad testId runId = do
  es <- sqliteLoadExecutionSteps testId runId
  init <- sqliteLoadInitDeployment testId
  pure $ mkTrace es init

sqliteStorage :: Storage IO
sqliteStorage = Storage { load = sqliteLoad }
