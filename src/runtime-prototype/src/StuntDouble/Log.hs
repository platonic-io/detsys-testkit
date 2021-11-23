{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
module StuntDouble.Log where

import Control.Monad (forM)
import Data.Aeson
import qualified Data.Aeson.Text as AesonText
import Data.Aeson.Encoding
       ( encodingToLazyByteString
       , list
       , pair
       , pairs
       , text
       , unsafeToEncoding
       )
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy.Char8 as LBS
import Data.Binary.Builder (fromLazyByteString)
import Data.Char (toLower)
import qualified Data.Text.Lazy as Text
import Data.Traversable (fmapDefault, foldMapDefault)
import Control.Exception
import GHC.Generics (Generic)

import StuntDouble.Codec
import StuntDouble.Message
import StuntDouble.Reference
import StuntDouble.Time
import StuntDouble.LogicalTime

------------------------------------------------------------------------

newtype Log = Log [Timestamped LogEntry]
  deriving stock (Show, Read)
  deriving newtype (FromJSON)

data Timestamped' lt a = Timestamped
 { tsContent :: a
 , tsLogicalTime :: lt
 , tsTime :: Time
 } deriving stock (Show, Read, Generic)
type Timestamped = Timestamped' LogicalTime

instance Functor (Timestamped' lt) where
  fmap = fmapDefault

instance Foldable (Timestamped' lt) where
  foldMap = foldMapDefault

instance Traversable (Timestamped' lt) where
  traverse f (Timestamped a b c) = (\x -> Timestamped x b c) <$> f a

tsAesonOptions = defaultOptions
  { fieldLabelModifier = \s -> case drop (length ("ts" :: String)) s of
      (x : xs) -> toLower x : xs
      [] -> error "impossible, unless the field names of `Timestamped` changed"
  }
instance (FromJSON lt, FromJSON a) => FromJSON (Timestamped' lt a)
  where parseJSON = genericParseJSON tsAesonOptions

data TimestampedLogically a = TimestampedLogically a LogicalTimeInt
  deriving stock (Show, Read)

data LogEntry' msg
  = LogSend
    { leLocalRef :: LocalRef
    , leRemoteRef :: RemoteRef
    , leMessage :: msg
    }
  | LogResumeContinuation
    { leRemoteRef :: RemoteRef
    , leLocalRef :: LocalRef
    , leMessage :: msg
    }
  deriving stock (Show, Read, Generic)
type LogEntry = LogEntry' Message

instance Functor LogEntry' where
  fmap = fmapDefault

instance Foldable LogEntry' where
  foldMap = foldMapDefault

instance Traversable LogEntry' where
  traverse f (LogSend a b c) = (\x -> LogSend a b x) <$> f c
  traverse f (LogResumeContinuation a b c) = (\x -> LogResumeContinuation a b x) <$> f c

leAesonOptions = defaultOptions
  { fieldLabelModifier = \s -> case drop (length ("le" :: String)) s of
      (x : xs) -> toLower x : xs
      [] -> error "impossible, unless the field names of `LogEntry` changed"
  , sumEncoding = defaultTaggedObject
    { tagFieldName = "tag" }
  }

instance FromJSON m => FromJSON (LogEntry' m)
  where parseJSON = genericParseJSON leAesonOptions

  {-
  = Spawned LocalRef
  | Turn TurnData
  | ClientRequestEntry
  | ClientResponseEntry
  | ErrorLogEntry SomeException

data TurnData = TurnData
  { tdActor         :: LocalRef
  , tdBeforeState   :: String -- XXX: State
  , tdMessage       :: Message
  , tdActions       :: [ActionLogEntry]
  , tdLogs          :: [LogLines]
  , tdAfterState    :: String -- XXX: State
  , tdLogicalTime   :: Int
  , tdSimulatedTime :: Int -- XXX: UTCTime
  , tdReply         :: Message
  }

data ActionLogEntry = XXX
data LogLines = YYY
-}

emptyLog :: Log
emptyLog = Log []

appendLog :: LogEntry -> LogicalTime -> Time -> Log -> Log
appendLog e lt t (Log es) = Log (Timestamped e lt t : es)

-- XXX: Use more efficient data structure to avoid having to reverse.
-- AdminTransport wants the messages to be `String`, otherwise we could probably skip
-- some translation here
getLog :: Codec -> Log -> String
getLog (Codec enc _) (Log es) =
  LBS.unpack $ encodingToLazyByteString f
  where
    f = flip list (reverse es) $ \(Timestamped x (LogicalTime _ lt) t) -> pairs
      ( (pair "content" $ pairs
          ((pair "tag" (case x of
            LogSend {} -> text "LogSend"
            LogResumeContinuation{} -> text "LogResumeContinuation")) <>
          "localRef" .= leLocalRef x <>
          "remoteRef" .= leRemoteRef x <>
          (pair "message" (unsafeToEncoding (fromLazyByteString (enc (leMessage x)))))
          )) <>
      "logicalTime" .= lt <>
      "time" .= t
      )

parseLog :: NodeName -> Codec -> ByteString -> Either String Log
parseLog nn (Codec _ dec) s = do
  xs :: [Timestamped' LogicalTimeInt (LogEntry' Value)] <- eitherDecode s
  xs' <- forM xs $ \t ->
    forM t $ \le ->
      forM le $ \m ->
        dec (encode m)
  pure . Log $ fmap (\ (Timestamped x lt t) -> Timestamped x (LogicalTime nn lt) t) xs'

-- Assumes the logs are already sorted
merge :: Log -> Log -> Log
merge (Log es) (Log es') = Log (go es es')
  where
    lt :: Timestamped a -> LogicalTime
    lt (Timestamped _ l _) = l
    go :: [Timestamped a] -> [Timestamped a] -> [Timestamped a]
    go [] es' = es'
    go es [] = es
    go es@(x:xs) es'@(y:ys)
      | HappenedBeforeOrConcurrently <- relation (lt x) (lt y) = x : go xs es'
      | otherwise = y : go es ys
  {-

type EventLog = [LogEntry]

data LogEntry
  = LogInvoke RemoteRef LocalRef Message Message EventLoopName
  | LogSendStart RemoteRef RemoteRef Message CorrelationId EventLoopName
  | LogSendFinish CorrelationId Message EventLoopName
  | LogRequest RemoteRef RemoteRef Message Message EventLoopName
  | LogReceive RemoteRef RemoteRef Message CorrelationId EventLoopName
  | LogRequestStart RemoteRef RemoteRef Message CorrelationId EventLoopName
  | LogRequestFinish CorrelationId Message EventLoopName
  | LogComment String EventLoopName
  | LogAsyncIOFinish CorrelationId IOResult EventLoopName
  deriving (Eq, Show)

isComment :: LogEntry -> Bool
isComment LogComment {} = True
isComment _otherwise    = False
-}
