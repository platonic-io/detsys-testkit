{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}

module StuntDouble.IO where

import Control.Monad
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Data.Hashable
import Data.IORef
import Data.Text
import Database.SQLite.Simple
import Database.SQLite.Simple.FromField
import Database.SQLite.Simple.FromRow
import Database.SQLite.Simple.Internal
import Database.SQLite.Simple.Ok

import StuntDouble.Datatype

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

  | IOExecute Query [NamedParam]
  | IOQuery Query [NamedParam]

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
  , ioExecute :: Query -> [NamedParam] -> m ()
  , ioQuery   :: Query -> [NamedParam] -> m [Datatype]
  }

data IOResult
  = IOValue Value
  | IOUnit ()
  | IOString String
  | IORows [Datatype]
  deriving Show

diskIO :: Monad m => IOOp -> Disk m -> m IOResult
diskIO (IOGet k)        io = IOValue <$> ioGet io k
diskIO (IOPut k v)      io = IOUnit <$> ioPut io k v
diskIO (IODelete k)     io = IOUnit <$> ioDelete io k
diskIO (IOPuts kvs)     io = IOUnit <$> ioPuts io kvs
diskIO (IODeletes ks)   io = IOUnit <$> ioDeletes io ks
diskIO IOIterate {}    _io = error "not implemented yet"
diskIO (IOExecute q ps) io = IOUnit <$> ioExecute io q ps
diskIO (IOQuery q ps)   io = IORows <$> ioQuery io q ps
diskIO (IOReturn x)    _io = return x

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
    , ioQuery   = \_ _ -> return [Bool False]
    }

slowFakeDisk :: IO (Disk IO)
slowFakeDisk = undefined

realSqlite :: FilePath -> IO (Disk IO)
realSqlite fp = do
  conn <- open fp
  return Disk
    { ioGet     = undefined
    , ioPut     = undefined
    , ioDelete  = undefined
    , ioPuts    = undefined
    , ioDeletes = undefined
    , ioIterate = undefined

    , ioExecute = executeNamed conn
    , ioQuery   = queryNamed conn
    }

instance FromRow Datatype where
  fromRow :: RowParser Datatype
  fromRow = do
    n <- numFieldsRemaining
    List <$> replicateM n field

instance FromField Datatype where
  fromField :: FieldParser Datatype
  fromField (Field (SQLInteger i) _) = Ok (Integer (fromIntegral i))
  fromField (Field (SQLFloat f) _)   = Ok (Double f)
  fromField (Field (SQLText t) _)    = Ok (Text t)
  fromField (Field (SQLBlob _bs) _)  = undefined -- XXX: Datatype doesn't have ByteStrings.
  fromField (Field SQLNull _)        = Ok None
