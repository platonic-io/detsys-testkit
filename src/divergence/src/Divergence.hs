{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
module Divergence where

import Control.Monad (zipWithM)
import qualified Data.ByteString.Lazy.Char8 as BSL
import Data.Monoid (First(..))
import Database.SQLite.Simple
import qualified Data.Aeson as Aeson
import Data.String (fromString)
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as Text
import qualified Data.Text.Lazy.Encoding as TextEncoding

import Control.Exception (throwIO)
import System.Environment (getEnv)
import System.FilePath ((</>))
import System.IO.Error (catchIOError, isDoesNotExistError)

type Json = Aeson.Value

data Difference
  = SingleDifference
    { event1Id :: Int
    , event2Id :: Int
    , event1 :: Json
    , event2 :: Json
    }
  | DifferentLength
    { len1 :: Int
    , len2 :: Int
    , diff :: (Maybe Difference)
    }
  deriving Show

prettyError :: Difference -> String
prettyError d = case d of
  SingleDifference i1 i2 e1 e2 ->
    "The first difference was between (event: " <> show i1 <> "):\n"
    <> fjs e1 <> "\n"
    <> "  second event (" <> show i2 <> ")\n"
    <> fjs e2
  DifferentLength l l' m ->
    "The two test have different amount of events:"
    <> "  First : " <> show l <> "\n"
    <> "  Second: " <> show l' <> "\n"
    <> case m of
         Nothing -> ""
         Just d -> prettyError d
  where
    fjs :: Aeson.Value -> String
    fjs = BSL.unpack . Aeson.encode

data RawEvent = RawEvent
  { reId :: Int
  , reEvent :: Text
  }

instance FromRow RawEvent where
  fromRow = RawEvent <$> field <*> field

data Event = Event
  { eId :: Int
  , eEvent :: Json
  }

getDbPath :: IO String
getDbPath = do
  getEnv "DETSYS_DB"
    `catchIOError` \(e :: catchIOError) ->
      if isDoesNotExistError e
        then do
          home <- getEnv "HOME"
          return (home </> ".detsys.db")
        else throwIO e

sqliteEvents :: Int -> IO [Event]
sqliteEvents testId = do
  path <- getDbPath
  conn <- open path
  map translate <$> queryNamed conn
    "SELECT id, json_object('event', event, \
    \                       'meta', json_remove(meta, '$.test-id'), \
    \                       'data', json_remove(data, '$.test-id')) \
    \FROM event_log \
    \WHERE json_extract(meta, '$.test-id')=:testId \
    \ORDER BY id ASC"
    [":testId" := testId]
  where
    translate :: RawEvent -> Event
    translate re = Event
      { eId = reId re
      , eEvent = case Aeson.decode (TextEncoding.encodeUtf8 (reEvent re)) of
          Nothing -> fromString $ "Can't decode event: " <> show (reId re)
          Just js -> js
      }


divergence :: Int -> Int -> IO (Maybe Difference)
divergence t1 t2 = do
  es1 <- sqliteEvents t1
  es2 <- sqliteEvents t2
  let
    diff = getFirst . mconcat $ zipWith check es1 es2
    len1 = length es1
    len2 = length es2
  if len1 == len2
    then return diff
    else return $ Just $ DifferentLength len1 len2 diff
  where
    check e1 e2
      | eEvent e1 == eEvent e2 = First $ Nothing
      | otherwise = First $ Just (SingleDifference (eId e1) (eId e2) (eEvent e1) (eEvent e2))
