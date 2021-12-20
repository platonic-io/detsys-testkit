{-# LANGUAGE MagicHash #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE UnboxedTuples #-}

module Journal.Internal.ByteBuffer where

import Control.Exception
import Data.IORef
import Foreign
import GHC.ForeignPtr
import GHC.Exts
import GHC.Types
import GHC.IO
import System.Posix.IO (openFd, defaultFileFlags, OpenMode(ReadWrite))

import Journal.Internal.Mmap

------------------------------------------------------------------------
-- * Types

type MBArray = MutableByteArray# RealWorld

data ByteBuffer = ByteBuffer
  { bbData     :: {-# UNPACK #-} !MBArray
  , bbCapacity :: {-# UNPACK #-} !Capacity
  , bbLimit    :: {-# UNPACK #-} !(IORef Limit)
  , bbPosition :: {-# UNPACK #-} !(IORef Position)
  , bbMark     :: {-# UNPACK #-} !(IORef Position)
  , bbSlice    :: {-# UNPACK #-} !(IORef Slice)
  }

newtype Capacity = Capacity Int
  deriving (Num, Integral, Real, Ord, Eq, Enum)

newtype Limit = Limit Int
  deriving (Num, Integral, Real, Ord, Eq, Enum)

newtype Position = Position Int
  deriving (Num, Integral, Real, Ord, Eq, Enum)

newtype Slice = Slice Int
  deriving (Num, Integral, Real, Ord, Eq, Enum)

------------------------------------------------------------------------

newByteBuffer :: MBArray -> Capacity -> Limit -> Position -> Maybe (IORef Slice)
              -> IO ByteBuffer
newByteBuffer mba# capa lim pos mSli
  = ByteBuffer mba# capa
  <$> newIORef lim
  <*> newIORef pos
  <*> newIORef (-1)
  <*> maybe (newIORef 0) return mSli

bbPtr :: ByteBuffer -> Ptr a
bbPtr (ByteBuffer mba# _ _ _ _ _) = Ptr (byteArrayContents# (unsafeCoerce# mba#))
{-# INLINE bbPtr #-}

getCapacity :: ByteBuffer -> Capacity
getCapacity = bbCapacity
{-# INLINE getCapacity #-}

readLimit :: ByteBuffer -> IO Limit
readLimit = readIORef . bbLimit
{-# INLINE readLimit #-}

writeLimit :: ByteBuffer -> Limit -> IO ()
writeLimit bb = writeIORef (bbLimit bb)
{-# INLINE writeLimit #-}

readPosition :: ByteBuffer -> IO Position
readPosition = readIORef . bbPosition
{-# INLINE readPosition #-}

writePosition :: ByteBuffer -> Position -> IO ()
writePosition bb = writeIORef (bbPosition bb)
{-# INLINE writePosition #-}

incrPosition :: ByteBuffer -> Int -> IO ()
incrPosition bb i = modifyIORef (bbPosition bb) (+ fromIntegral i)
{-# INLINE incrPosition #-}

readMark :: ByteBuffer -> IO Position
readMark = readIORef . bbMark
{-# INLINE readMark #-}

writeMark :: ByteBuffer -> Position -> IO ()
writeMark bb = writeIORef (bbMark bb)
{-# INLINE writeMark #-}

readSlice :: ByteBuffer -> IO Slice
readSlice = readIORef . bbSlice

writeSlice :: ByteBuffer -> Slice -> IO ()
writeSlice bb = writeIORef (bbSlice bb)

------------------------------------------------------------------------

remaining :: ByteBuffer -> IO Int
remaining bb = do
  lim <- readLimit bb
  pos <- readPosition bb
  return (fromIntegral (lim - fromIntegral pos))

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
          mark <= fromIntegral pos &&
          pos <= fromIntegral lim &&
          lim <= fromIntegral capa)
    (return ())

------------------------------------------------------------------------
-- * Create

allocate :: Int -> IO ByteBuffer
allocate capa@(I# capa#) = IO $ \s ->
  case newPinnedByteArray# capa# s of
    (# s', mba# #) -> unIO (newByteBuffer mba# (Capacity capa) (Limit capa) 0 Nothing) s'

allocateAligned :: Int -> Int -> IO ByteBuffer
allocateAligned capa@(I# capa#) align@(I# align#) = IO $ \s ->
  case newAlignedPinnedByteArray# capa# align# s of
    (# s', mba# #) -> unIO (newByteBuffer mba# (Capacity capa) (Limit capa) 0 Nothing) s'

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
wrap bb = newByteBuffer (bbData bb) capa lim (Position 0) (Just (bbSlice bb))
  where
    capa = bbCapacity bb
    lim  = Limit (fromIntegral capa)

wrapPart :: ByteBuffer -> Int -> Int -> IO ByteBuffer
wrapPart bb offset len = newByteBuffer (bbData bb) capa lim pos (Just (bbSlice bb))
  where
    capa = bbCapacity bb
    lim  = Limit (fromIntegral offset + fromIntegral len)
    pos  = Position (fromIntegral offset)

slice :: ByteBuffer -> IO ByteBuffer
slice bb = do
  pos <- readPosition bb
  left <- remaining bb
  slice <- newIORef (fromIntegral pos)
  newByteBuffer (bbData bb) (Capacity left) (Limit left) (Position 0) (Just slice)

duplicate :: ByteBuffer -> IO ByteBuffer
duplicate bb = do
  lim <- readLimit bb
  pos <- readPosition bb
  newByteBuffer (bbData bb) (getCapacity bb) lim pos (Just (bbSlice bb))

------------------------------------------------------------------------

mark :: ByteBuffer -> IO ()
mark bb = do
  pos <- readPosition bb
  writeMark bb pos

compact :: ByteBuffer -> IO ByteBuffer
compact = undefined

------------------------------------------------------------------------

clear :: ByteBuffer
clear = undefined

flipBB :: ByteBuffer -> IO ByteBuffer
flipBB = undefined

rewind :: ByteBuffer -> IO ByteBuffer
rewind = undefined

reset :: ByteBuffer -> IO ()
reset bb = do
  mrk <- readMark bb
  writePosition bb mrk

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

-- writeIntArray#

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
