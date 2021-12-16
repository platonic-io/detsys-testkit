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

readLimit :: ByteBuffer -> IO Int
readLimit = readIORef . bbLimit

readPosition :: ByteBuffer -> IO Int
readPosition = readIORef . bbPosition

writePosition :: ByteBuffer -> Int -> IO ()
writePosition bb = writeIORef (bbPosition bb)

incrPosition :: ByteBuffer -> Int -> IO ()
incrPosition bb i = modifyIORef (bbPosition bb) (+ i)

------------------------------------------------------------------------

remaining :: ByteBuffer -> IO Int
remaining bb = undefined


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
    (# s', mba# #) -> unIO (newByteBuffer mba# capa 0 0) s'

mmapped :: FilePath -> Int -> IO ByteBuffer
mmapped = undefined

wrap :: ByteBuffer -> IO ByteBuffer
wrap = undefined

wrapPart :: ByteBuffer -> Int -> Int -> IO ByteBuffer
wrapPart bb offset len = undefined

slice :: ByteBuffer -> IO ByteBuffer
slice bb@(ByteBuffer mba# _ _ _) = do
  pos <- readPosition bb
  -- XXX:
  newByteBuffer (unsafeCoerce# (bbPtr bb `plusPtr` pos)) undefined undefined 0

duplicate :: ByteBuffer -> IO ByteBuffer
duplicate bb@(ByteBuffer mba# _ _ _) = do
  pos <- readPosition bb
  lim <- readLimit bb
  newByteBuffer mba# (getCapacity bb) pos lim

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
-- * Mapped

msync :: ByteBuffer -> IO ()
msync = undefined
