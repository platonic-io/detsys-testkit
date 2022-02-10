{-# LANGUAGE DeriveGeneric #-}
module Snapshot where

import Data.Binary (Binary)
import qualified Data.Binary as Binary
import qualified Data.ByteString.Lazy as LBS
import GHC.Generics (Generic)

import qualified System.Directory as Dir
import qualified System.FilePath as FP
import System.IO (hClose, IOMode(ReadMode), openFile, openTempFile)

import StateMachine

data Snapshot = Snapshot
  { ssBytesInJournal :: Int
  , ssState :: InMemoryDumblog
  } deriving Generic

instance Binary Snapshot where

toFile :: Snapshot -> FilePath -> IO ()
toFile imd fp = do
  fp <- Dir.canonicalizePath fp
  let dir = FP.takeDirectory fp
  (fpt, h) <- openTempFile dir "SnapshotTemp"
  LBS.hPut h (Binary.encode imd)
  hClose h
  Dir.renameFile fpt fp

readFile :: FilePath -> IO (Maybe Snapshot)
readFile fp = do
  b <- Dir.doesFileExist fp
  if b then Just <$> Binary.decodeFile fp else pure Nothing
