{-# LANGUAGE ForeignFunctionInterface #-}
module Journal.Internal.FileAllocate
  (fileAllocate)
  where

import Foreign
import Foreign.C.Types

import System.Posix.Types

foreign import ccall unsafe "mac_fallocate"
  mac_fallocate :: CInt -> COff -> COff -> IO Int

fileAllocate :: Fd -> FileOffset -> FileOffset -> IO ()
fileAllocate fd off len = do
  res <- mac_fallocate (fromIntegral fd) off len
  if res == 0
    then return ()
    else error $ "fileAllocate " <> show fd <> " " <> show off <> " " <> show len
                <> " ;;; returned returned non-zero result: " <> show res
