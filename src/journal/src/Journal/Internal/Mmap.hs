module Journal.Internal.Mmap where

import Data.Bits ((.|.))
import Foreign.C.Error
import Data.Maybe (fromMaybe)
import Foreign.C.Types
import Foreign.Ptr
import System.Posix.Types

------------------------------------------------------------------------

foreign import ccall unsafe "sys/mman.h mmap"
  c_mmap :: Ptr a -> CSize -> CInt -> CInt -> CInt -> COff -> IO (Ptr a)

foreign import ccall unsafe "sys/mman.h munmap"
  c_munmap :: Ptr a -> CSize -> IO CInt

foreign import ccall unsafe "sys/mman.h msync"
  c_msync :: Ptr a -> CSize -> CInt -> IO CInt

mmap :: Maybe (Ptr a) -> CSize -> MemoryProtection -> MemoryVisibility -> Maybe Fd
     -> COff -> IO (Ptr a)
mmap mAddr len prot visib mFd offset =
  throwErrnoIf (== mAP_FAILED) "mmap" $
    c_mmap addr len (mpCInt prot) flags fd offset
  where
    mAP_FAILED = nullPtr `plusPtr` (-1)

    addr = fromMaybe nullPtr mAddr

    fd :: CInt
    fd = case mFd of
      Nothing        -> (-1)
      Just (Fd cint) -> cint

    flags :: CInt
    flags =  maybe mAP_ANONYMOUS (const 0) mFd
         .|. maybe 0 (const mAP_FIXED) mAddr
         .|. mvCInt visib

    mAP_ANONYMOUS :: CInt
    mAP_ANONYMOUS = 32

    mAP_FIXED :: CInt
    mAP_FIXED = 16

munmap :: Ptr a -> CSize -> IO ()
munmap addr len = throwErrnoIfMinus1_ "munmap" (c_munmap addr len)

msync
  :: Ptr a
  -> CSize
  -> MSyncFlag
  -> Bool -- ^ Asks to invalidate other mappings of the same file (so
          --   that they can be updated with the fresh values just
          --   written).
  -> IO ()
msync addr len flag invalidate = throwErrnoIfMinus1_ "msync" (c_msync addr len flags)
  where
    flags = msyncCInt flag .|. if invalidate then 2 else 0

data MSyncFlag
  = MS_ASYNC -- | Specifies that an update be scheduled, but the call
             --   returns immediately.
  | MS_SYNC  -- | Requests an update and waits for it to complete.

msyncCInt :: MSyncFlag -> CInt
msyncCInt MS_ASYNC      = 1
msyncCInt MS_SYNC       = 4

data MemoryProtection = PROT_NONE | PROT PROT_SOME
data PROT_SOME = READ | WRITE | EXEC | PROT_SOME :| PROT_SOME

mpCInt :: MemoryProtection -> CInt
mpCInt PROT_NONE   = 0
mpCInt (PROT some) = go some
  where
    go :: PROT_SOME -> CInt
    go READ     = 1
    go WRITE    = 2
    go EXEC     = 4
    go (l :| r) = go l .|. go r

data MemoryVisibility = MAP_SHARED | MAP_PRIVATE

mvCInt :: MemoryVisibility -> CInt
mvCInt MAP_SHARED  = 1
mvCInt MAP_PRIVATE = 2

------------------------------------------------------------------------

main :: IO ()
main = do
  ptr <- mmap Nothing 16 (PROT (READ :| WRITE)) MAP_SHARED Nothing 0
  msync ptr 16 MS_SYNC False
  munmap ptr 16
