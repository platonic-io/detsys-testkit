{-# LANGUAGE MagicHash #-}

module Journal.Internal.Utils where

import Control.Exception (assert, bracket)
import Data.Bits ((.|.))
import Data.Int (Int32, Int64)
import Foreign.Marshal.Alloc (callocBytes, free)
import GHC.Int (Int(I#), Int32(I32#), Int64(I64#))
import GHC.Stack (HasCallStack)
import System.Directory (canonicalizePath, getTemporaryDirectory)
import System.IO (Handle, hClose, openTempFile)
import System.Posix.Fcntl (fileAllocate)
import System.Posix.Files (ownerReadMode, ownerWriteMode)
import System.Posix.IO
       ( OpenMode(ReadWrite)
       , closeFd
       , defaultFileFlags
       , fdWriteBuf
       , openFd
       )
import System.Posix.Types (Fd)

------------------------------------------------------------------------

assertM :: (HasCallStack, Monad m) => Bool -> m ()
assertM b = assert b (return ())

withRWFd :: FilePath -> (Fd -> IO a) -> IO a
withRWFd fp k =
  bracket
    (openFd fp ReadWrite (Just (ownerReadMode .|. ownerWriteMode)) defaultFileFlags)
    closeFd
    k

int2Int32 :: Int -> Int32
int2Int32 i@(I# i#) = assert (i <= fromIntegral (maxBound :: Int32)) (I32# i#)

int2Int64 :: Int -> Int64
int2Int64 i@(I# i#) = assert (i <= fromIntegral (maxBound :: Int64)) (I64# i#)

fallocate :: FilePath -> Int -> IO ()
fallocate fp len = do
  withRWFd fp $ \fd -> do
    fileAllocate fd 0 (fromIntegral len)
    -- NOTE: `fileAllocate` only allocates the space it doesn't zero it,
    -- unlike `fallocate(1)`, so we do that next.
    bracket (callocBytes len) free $ \zeroesPtr -> do
      bytesWritten <- fdWriteBuf fd zeroesPtr (fromIntegral len)
      assertM (fromIntegral bytesWritten == len)

withTempFile :: String -> (FilePath -> Handle -> IO a) -> IO a
withTempFile name k = do
  tmp <- canonicalizePath =<< getTemporaryDirectory
  bracket (openTempFile tmp name) (\(_fp, h) -> hClose h) (uncurry k)
