{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE Rank2Types #-}

module StuntDouble.IO where

import Control.Monad
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Data.Hashable
import Data.IORef
import Data.Text
import Data.Typeable
import Database.SQLite.Simple

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
  | forall r. (Typeable r, FromRow r) => IOQuery (Proxy r) Query [NamedParam]

  -- | forall r. IOReturn (IOResult r)

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
  , ioQuery   :: forall r. FromRow r => Query -> [NamedParam] -> m [r]
  }

data IOResult
  = IOValue Value
  | IOUnit ()
  | IOString String
  | forall r. (Typeable r, FromRow r) => IORows (Proxy r) [r]

diskIO :: Monad m => IOOp -> Disk m -> m IOResult
diskIO (IOGet k)        io = IOValue <$> ioGet io k
diskIO (IOPut k v)      io = IOUnit <$> ioPut io k v
diskIO (IODelete k)     io = IOUnit <$> ioDelete io k
diskIO (IOPuts kvs)     io = IOUnit <$> ioPuts io kvs
diskIO (IODeletes ks)   io = IOUnit <$> ioDeletes io ks
diskIO IOIterate {}    _io = error "not implemented yet"
diskIO (IOExecute q ps) io = IOUnit <$> ioExecute io q ps
diskIO (IOQuery r q ps) io = IORows r <$> ioQuery io q ps
-- diskIO (IOReturn x)    _io = return x

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
    , ioQuery   = \_ _ -> undefined -- return [[1]]
{-
    * Couldn't match type `r' with `[Integer]'
      `r' is a rigid type variable bound by
        a type expected by the context:
          forall r. FromRow r => Query -> [NamedParam] -> IO [r]
        at src/StuntDouble/IO.hs:85:19-38
      Expected type: IO [r]
        Actual type: IO [[Integer]]
    * In the expression: return [[1]]
-}
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
    , ioQuery   = queryNamed   conn
    }
