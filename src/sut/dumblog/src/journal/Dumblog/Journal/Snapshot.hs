{-# LANGUAGE DeriveGeneric #-}
module Dumblog.Journal.Snapshot where

import Data.Binary (Binary)
import qualified Data.Binary as Binary
import qualified Data.ByteString.Lazy as LBS
import Data.Int (Int64)
import GHC.Generics (Generic)

import qualified System.Directory as Dir
import qualified System.FilePath as FP
import System.IO (hClose, openTempFile)

import Dumblog.Journal.StateMachine

data Snapshot = Snapshot
  { ssBytesInJournal :: Int
  , ssVersion :: Int64
  , ssState :: InMemoryDumblog
  } deriving Generic

instance Binary Snapshot

toFile :: Snapshot -> FilePath -> IO ()
toFile imd fp0 = do
  fp <- Dir.canonicalizePath fp0
  let dir = FP.takeDirectory fp
  (fpt, h) <- openTempFile dir "SnapshotTemp"
  LBS.hPut h (Binary.encode imd)
  hClose h
  Dir.renameFile fpt fp

readFile :: FilePath -> IO (Maybe Snapshot)
readFile fp = do
  b <- Dir.doesFileExist fp
  if b then Just <$> Binary.decodeFile fp else pure Nothing
