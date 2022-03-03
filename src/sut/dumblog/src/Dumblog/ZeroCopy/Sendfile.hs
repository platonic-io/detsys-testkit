{-# LANGUAGE ForeignFunctionInterface #-}

module Dumblog.ZeroCopy.Sendfile where

import Data.Int
import Data.Word
import System.Posix.Types (Fd(Fd))
import Foreign
import Foreign.C.Types
import Network.Socket (Socket, withFdSocket)

------------------------------------------------------------------------

foreign import ccall unsafe "sys/sendfile.h sendfile"
  c_sendfile :: Fd -> Fd -> Ptr Int64 -> Word64 -> IO Int64

sendfile :: Socket -> Fd -> Int64 -> Word64 -> IO Int64
sendfile outSock inFd offset len =
  alloca $ \offsetPtr -> do
    poke offsetPtr offset
    withFdSocket outSock $ \outFd ->
      c_sendfile (fromIntegral outFd) inFd offsetPtr len
