module Journal.Internal.Utils where

import Control.Exception (assert, bracket)
import Data.Int (Int32, Int64)
import Data.Bits ((.|.))
import GHC.Stack (HasCallStack)
import System.Posix.Files (ownerReadMode, ownerWriteMode)
import System.Posix.IO
       (OpenMode(ReadWrite), closeFd, defaultFileFlags, openFd)
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
int2Int32 i = assert (i <= fromIntegral (maxBound :: Int32)) (fromIntegral i)

int2Int64 :: Int -> Int64
int2Int64 i = assert (i <= fromIntegral (maxBound :: Int64)) (fromIntegral i)
