module Journal.Internal.FileAllocate
  (fileAllocate)
  where

import Foreign
import Foreign.C.Types

import System.Posix.Types
import qualified System.Posix.Fcntl as Posix

fileAllocate :: Fd -> FileOffset -> FileOffset -> IO ()
fileAllocate = Posix.fileAllocate
