{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}

module StuntDouble.IO where

import Control.Exception (finally)
import Control.Monad
import Data.String
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Data.Hashable
import Data.IORef
import Data.Text (Text)
import Data.ByteString (ByteString)
import qualified Database.SQLite3 as Base
import Database.SQLite.Simple
import Database.SQLite.Simple.FromField
import Database.SQLite.Simple.Ok
import Database.SQLite.Simple.Internal (Field(Field))


------------------------------------------------------------------------

newtype Key = Key Text
  deriving (Eq, Show, Hashable)

newtype Value = Value Text
  deriving Show

newtype Index = Index Int

data IOOp
  = IOGet Key
  | IOPut Key Value
  | IODelete Key
  | IOPuts [(Key, Value)]
  | IODeletes [Key]
  | IOIterate Key Key

  | IOExecute String [NamedParam]
  | IOExecuteMany String [[NamedParam]]
  | IOQuery String [NamedParam]

  | IOReturn IOResult

data Disk m = Disk
  -- LevelDB.
  { ioGet     :: Key -> m Value
  , ioPut     :: Key -> Value -> m ()
  , ioDelete  :: Key -> m ()
  , ioPuts    :: [(Key ,Value)] -> m ()
  , ioDeletes :: [Key] -> m ()
  , ioIterate :: Maybe Key -> Maybe Key -> m [(Key, Value)]

  -- SQLite.
  , ioExecute     :: String -> [NamedParam]   -> m ()
  , ioExecuteMany :: String -> [[NamedParam]] -> m ()
  , ioQuery       :: String -> [NamedParam] -> m [[FieldValue]]
  }

data FieldValue
  = FInt Int
  | FDouble Double
  | FText Text
  | FBlob ByteString
  | FNull
  deriving Show

instance FromField FieldValue where
  fromField (Field (SQLInteger i) _) = Ok (FInt (fromIntegral i))
  fromField (Field (SQLFloat f)   _) = Ok (FDouble f)
  fromField (Field (SQLText t)    _) = Ok (FText t)
  fromField (Field (SQLBlob b)    _) = Ok (FBlob b)
  fromField (Field SQLNull        _) = Ok FNull

data IOResult
  = IOValue Value
  | IOUnit ()
  | IOString String
  | IORows [[FieldValue]]
  deriving Show

diskIO :: Monad m => IOOp -> Disk m -> m IOResult
diskIO (IOGet k)            io = IOValue <$> ioGet io k
diskIO (IOPut k v)          io = IOUnit <$> ioPut io k v
diskIO (IODelete k)         io = IOUnit <$> ioDelete io k
diskIO (IOPuts kvs)         io = IOUnit <$> ioPuts io kvs
diskIO (IODeletes ks)       io = IOUnit <$> ioDeletes io ks
diskIO IOIterate {}        _io = error "not implemented yet"
diskIO (IOExecute q ps)     io = IOUnit <$> ioExecute io q ps
diskIO (IOExecuteMany q ps) io = IOUnit <$> ioExecuteMany io q ps
diskIO (IOQuery q ps)       io = IORows <$> ioQuery io q ps
diskIO (IOReturn x)        _io = return x

fakeDisk :: IO (Disk IO)
fakeDisk = do
  r <- newIORef HashMap.empty
  return Disk
    { ioGet     = \k ->   atomicModifyIORef' r (\hm -> (hm, hm HashMap.! k))
    , ioPut     = \k v -> atomicModifyIORef' r (\hm -> (HashMap.insert k v hm, ()))
    , ioDelete  = \k ->   atomicModifyIORef' r (\hm -> (HashMap.delete k hm, ()))
    , ioPuts    = mapM_ (\(k, v) -> atomicModifyIORef' r (\hm -> (HashMap.insert k v hm, ())))
    , ioDeletes = mapM_ (\k -> atomicModifyIORef' r (\hm -> (HashMap.delete k hm, ())))
    , ioIterate = undefined

    , ioExecute = \_ _ -> return ()
    , ioExecuteMany = \_ _ -> return ()
    , ioQuery   = \_ _ -> return [[FInt 1]]
    }

slowFakeDisk :: IO (Disk IO)
slowFakeDisk = undefined

realDisk :: FilePath -> IO (Disk IO)
realDisk fp = do
  conn <- open fp
  return Disk
    { ioGet     = undefined
    , ioPut     = undefined
    , ioDelete  = undefined
    , ioPuts    = undefined
    , ioDeletes = undefined
    , ioIterate = undefined

    , ioExecute     = \q -> executeNamed conn (fromString q)
    , ioExecuteMany = \q -> executeManyNamed conn (fromString q)
    , ioQuery       = \q -> queryNamed   conn (fromString q)
    }

class ParseRow a where
  parseRow :: [FieldValue] -> Maybe a

instance ParseRow [FieldValue] where
  parseRow :: [FieldValue] -> Maybe [FieldValue]
  parseRow = Just

instance ParseRow FieldValue where
  parseRow :: [FieldValue] -> Maybe FieldValue
  parseRow [fv] = Just fv
  parseRow _    = Nothing

parseRows :: ParseRow a => [[FieldValue]] -> Maybe [a]
parseRows = sequence . map parseRow

data DiskKind
  = FakeDisk
  | RealDisk FilePath

------------------------------------------------------------------------

-- XXX: Upstream to sqlite-simple?

executeManyNamed :: Connection -> Query -> [[NamedParam]] -> IO ()
executeManyNamed conn template paramRows = withStatement conn template $ \stmt -> do
  let Statement stmt' = stmt
  forM_ paramRows $ \params ->
    withBindNamed stmt params
      (void . Base.step $ stmt')

withBindNamed :: Statement -> [NamedParam] -> IO a -> IO a
withBindNamed stmt params io = do
  bindNamed stmt params
  io `finally` reset stmt
