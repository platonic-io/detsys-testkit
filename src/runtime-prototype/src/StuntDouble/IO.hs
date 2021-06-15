{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module StuntDouble.IO where

import Data.IORef
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Data.Text
import Data.Hashable

------------------------------------------------------------------------

newtype Key = Key Text
  deriving (Eq, Show, Hashable)

newtype Value = Value Text

newtype Index = Index Int

data IOOp
  = IOGet Key
  | IOPut Key Value
  | IODelete Key
  | IOPuts [(Key, Value)]
  | IODeletes [Key]
  | IOIterate Key Key

  | IOAppend Index Value
  | IORead Index

data Disk m = Disk
  -- LevelDB.
  { ioGet     :: Key -> m Value
  , ioPut     :: Key -> Value -> m ()
  , ioDelete  :: Key -> m ()
  , ioPuts    :: [(Key ,Value)] -> m ()
  , ioDeletes :: [Key] -> m ()
  , ioIterate :: Maybe Key -> Maybe Key -> m [(Key, Value)]

  -- SQLite event store.
  , ioAppend  :: Index -> Value -> m ()
  , ioRead    :: Index -> m Value
  }

data IOResult = IOValue Value | IOUnit ()

diskIO :: Monad m => IOOp -> Disk m -> m IOResult
diskIO (IOGet k)    io = IOValue <$> ioGet io k
diskIO (IOPut k v)  io = IOUnit <$> ioPut io k v
diskIO (IODelete k) io = IOUnit <$> ioDelete io k
diskIO _ _io = error "not implemented yet"

fakeDisk :: IO (Disk IO)
fakeDisk = do
  r <- newIORef HashMap.empty
  return Disk
    { ioGet     = \k ->   atomicModifyIORef' r (\hm -> (hm, hm HashMap.! k))
    , ioPut     = \k v -> atomicModifyIORef' r (\hm -> (HashMap.insert k v hm, ()))
    , ioDelete  = \k ->   atomicModifyIORef' r (\hm -> (HashMap.delete k hm, ()))
    , ioPuts    = undefined
    , ioDeletes = undefined
    , ioIterate = undefined

    -- SQLite event store.
    , ioAppend  = undefined
    , ioRead    = undefined
    }
