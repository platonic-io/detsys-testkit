{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}

module Journal.Internal.ByteBuffer where

import Control.Exception
import Data.IORef
import Foreign
import GHC.ForeignPtr
import GHC.Exts
import System.Posix.IO (openFd, defaultFileFlags, OpenMode(ReadWrite))

import Journal.Internal.Mmap

------------------------------------------------------------------------
-- * Type

data ByteBuffer = ByteBuffer
  { bbAddr     :: {-# UNPACK #-} !Addr#
  , bbCapacity :: {-# UNPACK #-} !Int
  , bbLimit    :: {-# UNPACK #-} !(IORef Int)
  , bbPosition :: {-# UNPACK #-} !(IORef Int)
  , bbMark     :: {-# UNPACK #-} !(IORef Int)
  }

newByteBuffer :: Addr# -> Int -> Int -> Int -> IO ByteBuffer
newByteBuffer addr# capa lim pos =
  ByteBuffer addr# capa <$> newIORef lim <*> newIORef pos <*> newIORef (-1)

bbPtr :: ByteBuffer -> Ptr a
bbPtr (ByteBuffer addr# _ _ _ _) = Ptr addr#
{-# INLINE bbPtr #-}

getCapacity :: ByteBuffer -> Int
getCapacity = bbCapacity
{-# INLINE getCapacity #-}

readLimit :: ByteBuffer -> IO Int
readLimit = readIORef . bbLimit
{-# INLINE readLimit #-}

writeLimit :: ByteBuffer -> Int -> IO ()
writeLimit bb = writeIORef (bbLimit bb)
{-# INLINE writeLimit #-}

readPosition :: ByteBuffer -> IO Int
readPosition = readIORef . bbPosition
{-# INLINE readPosition #-}

writePosition :: ByteBuffer -> Int -> IO ()
writePosition bb = writeIORef (bbPosition bb)
{-# INLINE writePosition #-}

incrPosition :: ByteBuffer -> Int -> IO ()
incrPosition bb i = modifyIORef (bbPosition bb) (+ i)
{-# INLINE incrPosition #-}

readMark :: ByteBuffer -> IO Int
readMark = readIORef . bbMark
{-# INLINE readMark #-}

writeMark :: ByteBuffer -> Int -> IO ()
writeMark bb = writeIORef (bbMark bb)
{-# INLINE writeMark #-}

------------------------------------------------------------------------

remaining :: ByteBuffer -> IO Int
remaining bb = do
  lim <- readLimit bb
  pos <- readPosition bb
  return (lim - pos)

------------------------------------------------------------------------
-- * Checks

boundCheck :: ByteBuffer -> Int -> IO ()
boundCheck bb ix = do
  if fromIntegral ix <= getCapacity bb
  then return ()
  else throwIO (IndexOutOfBounds "XXX")

invariant :: ByteBuffer -> IO ()
invariant bb = do
  mark <- readMark bb
  pos  <- readPosition bb
  lim  <- readLimit bb
  let capa = getCapacity bb
  assert ((mark == (-1) || 0 <= mark) &&
          mark <= pos &&
          pos <= lim &&
          lim <= capa)
    (return ())

------------------------------------------------------------------------
-- * Create

allocate :: Int -> IO ByteBuffer
allocate capa = do
  Ptr addr# <- mallocBytes capa
  newByteBuffer addr# capa capa 0

allocateAligned :: Int -> Int -> IO ByteBuffer
allocateAligned capa align = do
  Ptr addr# <- posixMemalign capa align
  newByteBuffer addr# capa capa 0

mmapped :: FilePath -> Int -> IO ByteBuffer
mmapped fp capa = do
  fd <- openFd fp ReadWrite Nothing defaultFileFlags
  pageSize <- sysconfPageSize
  bb <- allocateAligned capa pageSize
  ptr <- mmap (Just (bbPtr bb)) (fromIntegral capa)
           (PROT (READ :| WRITE)) MAP_SHARED (Just fd) 0
  assert (ptr == bbPtr bb) (return ())
  return bb

wrap :: ByteBuffer -> IO ByteBuffer
wrap bb = newByteBuffer (bbAddr bb) size size 0
  where
    size = bbCapacity bb

wrapPart :: ByteBuffer -> Int -> Int -> IO ByteBuffer
wrapPart bb offset len = newByteBuffer (bbAddr bb) size (offset + len) offset
  where
    size  = bbCapacity bb

slice :: ByteBuffer -> IO ByteBuffer
slice bb = do
  I# pos# <- readPosition bb
  left <- remaining bb
  newByteBuffer (bbAddr bb `plusAddr#` pos#) left left 0

duplicate :: ByteBuffer -> IO ByteBuffer
duplicate bb = do
  lim <- readLimit bb
  pos <- readPosition bb
  newByteBuffer (bbAddr bb) (getCapacity bb) lim pos

------------------------------------------------------------------------

compact :: ByteBuffer -> IO ByteBuffer
compact = undefined

------------------------------------------------------------------------

-- | The position is set to zero, the limit is set to the capacity, and the mark
-- is discarded.
clear :: ByteBuffer
clear = undefined

-- | The limit is set to the current position and then the position is set to
-- zero. If the mark is defined then it is discarded.
flipBB :: ByteBuffer -> IO ByteBuffer
flipBB = undefined

-- | Rewinds this buffer. The position is set to zero and the mark is discarded.
rewind :: ByteBuffer -> IO ByteBuffer
rewind = undefined

-- | Resets this buffer's position to the previously-marked position.
-- Invoking this method neither changes nor discards the mark's value.
reset :: ByteBuffer
reset = undefined

------------------------------------------------------------------------
-- * Single-byte relative and absolute operations

putByte :: ByteBuffer -> Word8 -> IO ()
putByte = undefined

getByte :: ByteBuffer -> IO Word8
getByte = undefined

putByteAt :: ByteBuffer -> Int -> Word8 -> IO ()
putByteAt = undefined

getByteAt :: ByteBuffer -> Int -> IO Word8
getByteAt = undefined

------------------------------------------------------------------------
-- * Multi-byte operations

putBytes :: ByteBuffer -> [Word8] -> IO ()
putBytes = undefined

getBytes :: ByteBuffer -> IO [Word8]
getBytes = undefined

------------------------------------------------------------------------
-- * Relative operations on `Storable` elements

putStorable :: Storable a => ByteBuffer -> a -> IO ()
putStorable bb x = do
  pos <- readPosition bb
  putStorableAt bb (fromIntegral pos) x
  incrPosition bb (sizeOf x)

getStorable :: Storable a => ByteBuffer -> IO a
getStorable bb = do
  pos <- readPosition bb
  x <- getStorableAt bb (fromIntegral pos)
  incrPosition bb (sizeOf x)
  return x

------------------------------------------------------------------------
-- * Absolute operations on `Storable` elements

putStorableAt :: Storable a => ByteBuffer -> Int -> a -> IO ()
putStorableAt bb ix x = do
  boundCheck bb ix
  pokeByteOff (bbPtr bb) ix x

getStorableAt :: Storable a => ByteBuffer -> Int -> IO a
getStorableAt bb ix = do
  boundCheck bb ix
  peekByteOff (bbPtr bb) ix

------------------------------------------------------------------------

-- indexCharOffAddr#
-- indexWideCharOffAddr#
-- indexIntOffAddr#
-- indexWordOffAddr#
-- indexAddrOffAddr#
-- indexFloatOffAddr#
-- indexDoubleOffAddr#
-- indexStablePtrOffAddr#
-- indexInt8OffAddr#
-- indexInt16OffAddr#
-- indexInt32OffAddr#
-- indexInt64OffAddr#
-- indexWord8OffAddr#
-- indexWord16OffAddr#
-- indexWord32OffAddr#
-- indexWord64OffAddr#

------------------------------------------------------------------------
-- * Mapped

-- | Calls `msync` which forces the data in memory to be synced to disk.
force :: ByteBuffer -> IO ()
force = undefined

------------------------------------------------------------------------

t :: IO ()
t = do
  bb <- mmapped "/tmp/mmap.txt" 4096
  bb' <- duplicate bb
  putStorable bb (0.1 :: Double)
  putStorable bb 'A'
  d <- getStorable bb'
  print (d :: Double)
