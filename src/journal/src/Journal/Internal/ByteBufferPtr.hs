{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}

module Journal.Internal.ByteBufferPtr where

import Control.Exception
import Control.Monad
import Data.Bits
import qualified Data.ByteString as BS
import qualified Data.ByteString.Internal as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Lazy.Internal as LBS
import Data.IORef
import Data.Int
import Data.Word
import Foreign (withForeignPtr)
import Foreign.Concurrent
import Foreign.Storable
import GHC.Exts
import GHC.ForeignPtr
import GHC.Stack
import GHC.Types
import System.Posix.IO
       (OpenMode(ReadWrite), closeFd, defaultFileFlags, openFd)

import Journal.Internal.Mmap

------------------------------------------------------------------------
-- * Types

data ByteBuffer = ByteBuffer
  { bbData     :: {-# UNPACK #-} !(ForeignPtr Word8)
  , bbCapacity :: {-# UNPACK #-} !Capacity
  , bbLimit    :: {-# UNPACK #-} !(IORef Limit)
  , bbPosition :: {-# UNPACK #-} !(IORef Position)
  , bbMark     :: {-# UNPACK #-} !(IORef Position)
  , bbSlice    :: {-# UNPACK #-} !(IORef Slice)
  -- XXX: ByteOrder / Endianess?
  }

newtype Capacity = Capacity { unCapacity :: Int }
  deriving (Num, Integral, Real, Ord, Eq, Enum)

newtype Limit = Limit Int
  deriving (Num, Integral, Real, Ord, Eq, Enum)

newtype Position = Position { unPosition :: Int }
  deriving (Num, Integral, Real, Ord, Eq, Enum)

newtype Slice = Slice Int
  deriving (Num, Integral, Real, Ord, Eq, Enum)

------------------------------------------------------------------------

newByteBuffer :: ForeignPtr Word8 -> Capacity -> Limit -> Position -> Maybe (IORef Slice)
              -> IO ByteBuffer
newByteBuffer fptr capa lim pos mSli
  = ByteBuffer fptr capa
  <$> newIORef lim
  <*> newIORef pos
  <*> newIORef (-1)
  <*> maybe (newIORef 0) return mSli

bbPtr :: ByteBuffer -> ForeignPtr Word8
bbPtr (ByteBuffer fptr _ _ _ _ _) = fptr
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

boundCheck :: HasCallStack => ByteBuffer -> Int -> IO ()
boundCheck bb ix = do
  -- XXX: ix + slice?
  -- XXX: parametrise on build flag and only do these checks if enabled?
  if ix < fromIntegral (getCapacity bb)
  then return ()
  else do
    putStrLn (prettyCallStack callStack)
    throwIO (IndexOutOfBounds errMsg)
  where
    errMsg = concat
      [ "boundCheck: index out of bounds "
      , "(", show ix, ",", show (unCapacity (getCapacity bb)), ")"
      ]

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

-- allocate :: Int -> IO ByteBuffer
-- allocate capa@(I# capa#) = IO $ \s ->
--   case newPinnedByteArray# capa# s of
--     (# s', mba# #) -> unIO (newByteBuffer mba# (Capacity capa) (Limit capa) 0 Nothing) s'

allocateAligned :: Int -> Int -> IO ByteBuffer
allocateAligned size align = do
  fptr <- posixMemalignFPtr size align
  newByteBuffer fptr (Capacity size) (Limit size) 0 Nothing

mmapped :: FilePath -> Int -> IO ByteBuffer
mmapped fp capa =
  bracket (openFd fp ReadWrite Nothing defaultFileFlags) closeFd $ \fd -> do
    pageSize <- sysconfPageSize
    ptr <- mmap Nothing (fromIntegral capa)
             (pROT_READ .|. pROT_WRITE) mAP_SHARED (Just fd) 0
    fptr <- newForeignPtr ptr (finalizer ptr pageSize)
    newByteBuffer fptr (Capacity pageSize) (Limit pageSize) 0 Nothing
  where
    finalizer :: Ptr a -> Int -> IO ()
    finalizer ptr size = munmap ptr (fromIntegral size)

wrap :: ByteBuffer -> IO ByteBuffer
wrap bb = newByteBuffer (bbData bb) capa lim (Position 0) (Just (bbSlice bb))
  where
    capa = bbCapacity bb
    lim  = Limit (fromIntegral capa)

wrapPart :: ByteBuffer -> Int -> Int -> IO ByteBuffer
wrapPart bb offset len = newByteBuffer (bbData bb) capa lim pos (Just (bbSlice bb))
  where
    capa = Capacity len
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

-- | Clears the byte buffer. The position is set to zero, the limit is set to
-- the capacity, and the mark is discarded.
clear :: ByteBuffer -> IO ()
clear bb = do
  writePosition bb 0
  let Capacity capa = getCapacity bb
  writeLimit bb (Limit capa)
  writeMark bb (-1)

  {-
clean :: ByteBuffer -> IO ()
clean bb = do
  Position (I# from#) <- readPosition bb
  Limit (I# to#) <- readLimit bb
  IO $ \s -> case setByteArray# (bbData bb) from# (to# -# from#) 0# s of
    s' -> (# s', () #)
-}

-- | Flips the byte buffer. The limit is set to the current position and then
-- the position is set to zero. If the mark is defined then it is discarded.
flipBB :: ByteBuffer -> IO ()
flipBB bb = do
  Position pos <- readPosition bb
  writeLimit bb (Limit pos)
  writePosition bb 0
  writeMark bb (-1)

-- | Rewinds the byte buffer. The position is set to zero and the mark is
-- discarded.
rewind :: ByteBuffer -> IO ()
rewind bb = do
  writePosition bb 0
  writeMark bb (-1)

-- | Resets the byte buffer's position to the previously marked position.
reset :: ByteBuffer -> IO ()
reset bb = do
  mrk <- readMark bb
  writePosition bb mrk

------------------------------------------------------------------------
-- * Single-byte relative and absolute operations

putByte :: ByteBuffer -> Word8 -> IO ()
putByte = undefined

getByte :: ByteBuffer -> IO Word8
getByte bb = do
  pos <- readPosition bb
  w8 <- indexWord8OffAddr bb (unPosition pos)
  writePosition bb (pos + 1)
  return w8

putByteAt :: ByteBuffer -> Int -> Word8 -> IO ()
putByteAt = undefined

getByteAt :: ByteBuffer -> Int -> IO Word8
getByteAt = undefined

------------------------------------------------------------------------
-- * Multi-byte relative and absolute operations

  {-
putBytes :: ByteBuffer -> ByteBuffer -> IO ()
putBytes src dest = do
  Position (I# destPos#) <- readPosition dest
  let Capacity srcCapa@(I# srcCapa#) = getCapacity src
  -- XXX: bounds check
  IO $ \s ->
    case copyMutableByteArray# (bbData src) 0# (bbData dest) destPos# srcCapa# s of
      s' -> (# s', () #)
  incrPosition dest srcCapa

getBytes :: ByteBuffer -> Int -> Int -> IO [Word8]
getBytes bb offset len = undefined

putByteString :: ByteBuffer -> BS.ByteString -> IO ()
putByteString bb bs = do
  let (fptr, I# offset#, I# len#) = BS.toForeignPtr bs
  boundCheck bb (I# (len# -# 1#))
  withForeignPtr fptr $ \(Ptr addr#) -> IO $ \s ->
    case copyAddrToByteArray# addr# (bbData bb) offset# len# s of
      s' -> (# s', () #)

putLazyByteString :: ByteBuffer -> LBS.ByteString -> IO ()
putLazyByteString bb lbs = do
  let (fptr, I# offset#, I# len#) = BS.toForeignPtr (LBS.toStrict lbs)
  boundCheck bb (I# (len# -# 1#))
  withForeignPtr fptr $ \(Ptr addr#) -> IO $ \s ->
    case copyAddrToByteArray# addr# (bbData bb) offset# len# s of
      s' -> (# s', () #)

getByteString :: ByteBuffer -> Int -> IO BS.ByteString
getByteString bb len@(I# len#) = do
  boundCheck bb (len - 1)
  Position (I# offset#) <- readPosition bb
  bs <- BS.create len $ \(Ptr addr#) -> IO $ \s ->
    case copyMutableByteArrayToAddr# (bbData bb) offset# addr# len# s of
      s' -> (# s', () #)
  incrPosition bb len
  return bs

getLazyByteString :: ByteBuffer -> Int -> IO LBS.ByteString
getLazyByteString bb len = do
  bs <- getByteString bb len
  return (LBS.fromStrict bs)

getByteStringAt :: ByteBuffer -> Int -> Int -> IO BS.ByteString
getByteStringAt bb offset len = do
  undefined
-}

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
  withForeignPtr (bbPtr bb) $ \ptr ->
    pokeByteOff ptr ix x

getStorableAt :: Storable a => ByteBuffer -> Int -> IO a
getStorableAt bb ix = do
  boundCheck bb ix
  withForeignPtr (bbPtr bb) $ \ptr ->
    peekByteOff ptr ix

------------------------------------------------------------------------

{-

-- readCharOffArray#
-- readWideCharOffArray#
readIntOffArrayIx :: ByteBuffer -> Int -> IO Int
readIntOffArrayIx bb ix@(I# ix#) = do
  boundCheck bb ix
  IO $ \s ->
    case readIntArray# (bbData bb) ix# s of
      (# s', i #) -> (# s', I# i #)
-- readWordOffArray#
-- readArrayOffAddr#
-- readFloatOffArray#
-- readDoubleOffArray#
-- readStablePtrOffArray#
-- readInt8OffArray#
-- readInt16OffArray#
readInt32OffArrayIx :: ByteBuffer -> Int -> IO Int32
readInt32OffArrayIx bb ix@(I# ix#) = do
  boundCheck bb ix
  IO $ \s ->
    case readInt32Array# (bbData bb) ix# s of
      (# s', i #) -> (# s', fromIntegral (I# i) #)

readInt64OffArrayIx :: ByteBuffer -> Int -> IO Int64
readInt64OffArrayIx bb ix@(I# ix#) = do
  boundCheck bb ix
  IO $ \s ->
    case readInt64Array# (bbData bb) ix# s of
      (# s', i #) -> (# s', fromIntegral (I# i) #)
-}

indexWord8OffAddr :: ByteBuffer -> Int -> IO Word8
indexWord8OffAddr bb offset@(I# offset#) = do
  boundCheck bb offset
  withForeignPtr (bbPtr bb) $ \(Ptr addr#) ->
    return (fromIntegral (W# (indexWord8OffAddr# addr# offset#)))

  {-
-- readWord16OffArray#
-- readWord32OffArray#
-- readWord64OffArray#

-- writeCharOffArray#
-- writeWideCharOffArray#
writeInt = writeIntOffArrayIx

writeIntOffArrayIx :: ByteBuffer -> Int -> Int -> IO ()
writeIntOffArrayIx bb ix@(I# ix#) (I# value#) = do
  boundCheck bb ix
  IO $ \s ->
    case writeIntArray# (bbData bb) ix# value# s of
      s' -> (# s', () #)
-- writeWordOffArray#
-- writeArrayOffAddr#
-- writeFloatOffArray#
-- writeDoubleOffArray#
-- writeStablePtrOffArray#
-- writeInt8OffArray#
-- writeInt16OffArray#

writeInt32OffArrayIx :: ByteBuffer -> Int -> Int32 -> IO ()
writeInt32OffArrayIx bb ix@(I# ix#) value = do
  boundCheck bb ix
  IO $ \s ->
    case writeInt32Array# (bbData bb) ix# value# s of
      s' -> (# s', () #)
  where
    I# value# = fromIntegral value

writeInt64OffArrayIx :: ByteBuffer -> Int -> Int64 -> IO ()
writeInt64OffArrayIx bb ix@(I# ix#) value = do
  boundCheck bb ix
  IO $ \s ->
    case writeInt64Array# (bbData bb) ix# value# s of
      s' -> (# s', () #)
  where
    I# value# = fromIntegral value

-- writeWord8OffArray#
-- writeWord16OffArray#
writeWord32OffArrayIx :: ByteBuffer -> Int -> Word32 -> IO ()
writeWord32OffArrayIx bb offset@(I# offset#) value = do
  boundCheck bb offset
  IO $ \s ->
    case writeWord32Array# (bbData bb) offset# value# s of
      s' -> (# s', () #)
  where
    W# value# = fromIntegral value
-- writeWord64OffArray#

-- atomicReadIntArray#
-- atomicWriteIntArray#

-- | Given a bytebuffer, an offset in machine words, the expected old value, and
-- the new value, perform an atomic compare and swap i.e. write the new value if
-- the current value matches the provided old value. Returns a boolean
-- indicating whether the compare and swap succeded or not. Implies a full
-- memory barrier.
casIntArray :: ByteBuffer -> Int -> Int -> Int -> IO Bool
casIntArray bb offset@(I# offset#) (I# old#) (I# new#) = do
  boundCheck bb offset
  IO $ \s ->
    case casIntArray# (bbData bb) offset# old# new# s of
      (# s', before# #) -> case before# ==# old# of
        1# -> (# s', True #)
        0# -> (# s', False #)

-- | Given a bytebuffer, and offset in machine words, and a value to add,
-- atomically add the value to the element. Returns the value of the element
-- before the operation. Implies a full memory barrier.
fetchAddIntArray :: ByteBuffer -> Int -> Int -> IO Int
fetchAddIntArray bb offset@(I# offset#) (I# incr#) = do
  boundCheck bb offset
  IO $ \s ->
    case fetchAddIntArray# (bbData bb) offset# incr# s of
      (# s', before# #) -> (# s', I# before# #)

-- | Given a bytebuffer, and offset in machine words, and a value to add,
-- atomically add the value to the element. Implies a full memory barrier.
fetchAddIntArray_ :: ByteBuffer -> Int -> Int -> IO ()
fetchAddIntArray_ bb offset@(I# offset#) (I# incr#) = do
  boundCheck bb offset
  IO $ \s ->
    case fetchAddIntArray# (bbData bb) offset# incr# s of
      (# s', _before# #) -> (# s', () #)

-- | Given a bytebuffer, and offset in machine words, and a value to add,
-- atomically add the value to the element. Returns the value of the element
-- after the operation. Implies a full memory barrier.
fetchAddWordAddr' :: ByteBuffer -> Int -> Word -> IO Int
fetchAddWordAddr' bb offset@(I# offset#) (W# incr#) = do
  boundCheck bb offset
  IO $ \s ->
    case fetchAddIntArray# (bbData bb) offset# incr# s of
      (# s', before# #) -> (# s', I# (before# +# incr#) #)
  -}

------------------------------------------------------------------------
-- * Mapped

-- | Calls `msync` which forces the data in memory to be synced to disk.
force :: ByteBuffer -> IO ()
force bb =
  withForeignPtr (bbPtr bb) $ \ ptr ->
    msync ptr (fromIntegral (bbCapacity bb)) mS_SYNC False

------------------------------------------------------------------------

t2 :: IO ()
t2 = do
  ds <- replicateM 200000 $ do
    bb <- mmapped "/tmp/mmap.txt" 4096
    bb' <- duplicate bb
    putStorable bb (0.1 :: Double)
    putStorable bb 'A'
    d <- getStorable bb'
    return (d :: Double)
  if all (== 0.1) ds
  then putStrLn "success!"
  else error "t2: bad"
