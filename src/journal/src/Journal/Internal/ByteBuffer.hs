{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}

module Journal.Internal.ByteBuffer where

import Control.Exception
import Data.IORef
import GHC.Word
import Foreign
import GHC.Exts
import GHC.Base (unIO)
import GHC.Types

------------------------------------------------------------------------
-- * Type

data ByteBuffer = ByteBuffer
  { bbArray    :: {-# UNPACK #-} !(MutableByteArray# RealWorld)
  , bbCapacity :: {-# UNPACK #-} !Int
  , bbLimit    :: {-# UNPACK #-} !(IORef Int)
  -- , bbMark     :: {-# UNPACK #-} !(IORef Int)
  , bbPosition :: {-# UNPACK #-} !(IORef Int)
  }

newByteBuffer :: MutableByteArray# RealWorld -> Int -> Int -> Int -> IO ByteBuffer
newByteBuffer mba# capa lim pos =
  ByteBuffer mba# capa <$> newIORef lim <*> newIORef pos

bbPtr :: ByteBuffer -> Ptr a
bbPtr (ByteBuffer mba# _ _ _) = Ptr (byteArrayContents# (unsafeCoerce# mba#))
{-# INLINE bbPtr #-}

getCapacity :: ByteBuffer -> Int
getCapacity = bbCapacity
{-# INLINE getCapacity #-}

readLimit :: ByteBuffer -> IO Int
readLimit = readIORef . bbLimit
{-# INLINE readLimit #-}

readPosition :: ByteBuffer -> IO Int
readPosition = readIORef . bbPosition
{-# INLINE readPosition #-}

writePosition :: ByteBuffer -> Int -> IO ()
writePosition bb = writeIORef (bbPosition bb)
{-# INLINE writePosition #-}

incrPosition :: ByteBuffer -> Int -> IO ()
incrPosition bb i = modifyIORef (bbPosition bb) (+ i)
{-# INLINE incrPosition #-}

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

------------------------------------------------------------------------
-- * Create

allocate :: Int -> IO ByteBuffer
allocate capa@(I# capa#) = IO $ \s ->
  case newPinnedByteArray# capa# s of
    (# s', mba# #) -> unIO (newByteBuffer mba# capa capa 0) s'

mmapped :: FilePath -> Int -> IO ByteBuffer
mmapped = undefined

wrap :: ByteBuffer -> IO ByteBuffer
wrap bb = newByteBuffer (bbArray bb) size size 0
  where
    mba# = bbArray bb
    size = I# (sizeofMutableByteArray# mba#)

wrapPart :: ByteBuffer -> Int -> Int -> IO ByteBuffer
wrapPart bb offset len = newByteBuffer mba# size (offset + len) offset
  where
    mba# = bbArray bb
    size = I# (sizeofMutableByteArray# mba#)

slice :: ByteBuffer -> IO ByteBuffer
slice bb@(ByteBuffer mba# _ _ _) = do
  pos <- readPosition bb
  left <- remaining bb
  putStrLn ("slice, pos: " ++ show pos ++ ", left: " ++ show left)
  putStrLn ("bbPtr: " ++ show (bbPtr bb))
  putStrLn ("bbPtr + pos: " ++ show (bbPtr bb `plusPtr` pos))
  print (Ptr (byteArrayContents# (unsafeCoerce# (bbPtr bb `plusPtr` pos))))
  newByteBuffer (unsafeCoerce# (bbPtr bb `plusPtr` pos)) left left 0

duplicate :: ByteBuffer -> IO ByteBuffer
duplicate bb@(ByteBuffer mba# _ _ _) = do
  lim <- readLimit bb
  pos <- readPosition bb
  newByteBuffer mba# (getCapacity bb) lim pos

------------------------------------------------------------------------

compact :: ByteBuffer -> IO ByteBuffer
compact = undefined

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

-- | The position is set to zero, the limit is set to the capacity, and the mark
-- is discarded.
clear :: ByteBuffer
clear = undefined

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
