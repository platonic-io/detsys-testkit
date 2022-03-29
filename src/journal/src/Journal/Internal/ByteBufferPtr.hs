{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Journal.Internal.ByteBufferPtr where

import Control.Exception
import Control.Monad
import Data.Bits
import qualified Data.ByteString as BS
import qualified Data.ByteString.Internal as BS
import qualified Data.ByteString.Unsafe as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Lazy.Internal as LBS
import Data.IORef
import Data.Int
import Data.Word
import Foreign (copyBytes, fillBytes, plusPtr, withForeignPtr, castPtr)
import Foreign.Concurrent
import Foreign.Storable
import GHC.Exts
import GHC.ForeignPtr
import GHC.IO (IO(IO))
import GHC.Int (Int32(I32#), Int64(I64#))
import GHC.Word (Word8(W8#), Word16(W16#), Word32(W32#), Word64(W64#))
import GHC.Stack
import System.Posix.IO
       (OpenMode(ReadWrite), closeFd, defaultFileFlags, openFd)

import Journal.Internal.Atomics
import Journal.Internal.Mmap
import Journal.Internal.Utils

------------------------------------------------------------------------
-- * Types

data ByteBuffer = ByteBuffer
  { bbData     :: {-# UNPACK #-} !(ForeignPtr Word8)
  , bbCapacity :: {-# UNPACK #-} !Capacity
  , bbLimit    :: {-# UNPACK #-} !Limit
  , bbSlice    :: {-# UNPACK #-} !Slice
  -- XXX: ByteOrder / Endianess? Use `Data.Bits.compliment` to reverse bits?
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

newByteBuffer :: ForeignPtr Word8 -> Capacity -> Limit -> Maybe Slice -> ByteBuffer
newByteBuffer fptr capa lim mSli = ByteBuffer fptr capa lim (maybe 0 id mSli)

-- This is a hack, don't use.
unsafeFromBS :: BS.ByteString -> ByteBuffer
unsafeFromBS bs = ByteBuffer fptr (Capacity len) (Limit len) (Slice offset)
  where
    (fptr, offset, len) = BS.toForeignPtr bs

getCapacity :: ByteBuffer -> Capacity
getCapacity = bbCapacity
{-# INLINE getCapacity #-}

------------------------------------------------------------------------
-- * Checks
checkInRange :: HasCallStack => Int -> (Int, Int) -> String -> IO ()
checkInRange val (lower, upper) name
  | val < lower = throwIO $ IndexOutOfBounds $ errMsg "below" lower
  | val > upper = throwIO $ IndexOutOfBounds $ errMsg "above" upper
  | otherwise = pure ()
  where
    errMsg what who = concat
      [ prettyCallStack callStack
      , "\ncheckInRange: index(", name , ") out of bounds "
      , show val, " is ", what, " ", show who, " in the range [", show lower
      , ", ", show upper, "]"
      ]

-- XXX: We probably want to be able to disable boundCheck at compile time.
boundCheck :: HasCallStack => ByteBuffer -> Int -> Int -> IO ()
boundCheck bb ix size = return ()
{-# INLINE boundCheck #-}

{- do
  invariant bb
  -- XXX: use Word for size?
  -- XXX: parametrise on build flag and only do these checks if enabled?
  Limit limit <- readIORef (bbLimit bb)
  if 0 <= size &&
     0 <= ix &&
     ix + size - 1 < limit
  then return ()
  else throwIO (IndexOutOfBounds $ errMsg limit)
  where
    errMsg limit = concat
      [ prettyCallStack callStack
      , "\nboundCheck: index out of bounds "
      , show [ix, ix + size - 1] , " isn't in [0," , show limit, ")"
      ]

invariant :: HasCallStack => ByteBuffer -> IO ()
invariant bb = do
  Position mark <- readMark bb
  Position pos <- readPosition bb
  Slice slice <- readIORef (bbSlice bb)
  Limit lim  <- readLimit bb
  let Capacity capa = getCapacity bb
  assertM (mark == (-1) || 0 <= mark)
  assertM (mark <= pos)
  assertM (pos <= lim)
  assertM (lim - slice <= capa)
-}

------------------------------------------------------------------------
-- * Create

allocate :: Int -> IO ByteBuffer
allocate size = do
  fptr <- mallocForeignPtrBytes size
  return (newByteBuffer fptr (Capacity size) (Limit size) Nothing)

allocateAligned :: Int -> Int -> IO ByteBuffer
allocateAligned size align = do
  fptr <- posixMemalignFPtr size align
  return (newByteBuffer fptr (Capacity size) (Limit size) Nothing)

mmapped :: FilePath -> Int -> IO ByteBuffer
mmapped fp size =
  bracket (openFd fp ReadWrite Nothing defaultFileFlags) closeFd $ \fd -> do
    -- pageSize <- sysconfPageSize -- XXX align with `pageSize`?
    ptr <- mmap Nothing (fromIntegral size)
             (pROT_READ .|. pROT_WRITE) mAP_SHARED (Just fd) 0
    fptr <- newForeignPtr ptr (finalizer ptr size)
    return (newByteBuffer fptr (Capacity size) (Limit size) Nothing)
  where
    finalizer :: Ptr a -> Int -> IO ()
    finalizer ptr size = munmap ptr (fromIntegral size)

wrap :: ByteBuffer -> ByteBuffer
wrap bb = newByteBuffer (bbData bb) capa lim (Just (bbSlice bb))
  where
    capa = bbCapacity bb
    lim  = Limit (fromIntegral capa)

wrapPart :: ByteBuffer -> Int -> Int -> ByteBuffer
wrapPart bb offset len =
  let Limit oldLimit = bbLimit bb
      Slice slice = bbSlice bb
  -- checkInRange offset (0, oldLimit) "offset"
  -- checkInRange len (0, oldLimit-offset) "len"
      slice' = Slice (slice + offset)
  in
    newByteBuffer (bbData bb) capa lim (Just slice')
  where
    capa = Capacity len
    lim  = Limit len
    pos  = Position 0

duplicate :: ByteBuffer -> ByteBuffer
duplicate bb =
  newByteBuffer (bbData bb) (getCapacity bb) (bbLimit bb) (Just (bbSlice bb))

------------------------------------------------------------------------

clean :: ByteBuffer -> IO ()
clean bb = do
  let Limit to = bbLimit bb
  fillBytes (unsafeForeignPtrToPtr (bbData bb)) 0 to

cleanAt :: ByteBuffer -> Int -> Int -> IO ()
cleanAt bb offset len = do
  boundCheck bb offset len
  let Slice slice = bbSlice bb
  fillBytes (unsafeForeignPtrToPtr (bbData bb) `plusPtr` (slice + offset)) 0 len

------------------------------------------------------------------------
-- * Multi-byte absolute operations

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
-}

putByteStringAt :: ByteBuffer -> Int -> BS.ByteString -> IO ()
putByteStringAt bb doffset bs = do
  let Slice slice = bbSlice bb
  let (fptr, soffset, len) = BS.toForeignPtr bs
  boundCheck bb doffset len
  withForeignPtr fptr $ \sptr ->
    withForeignPtr (bbData bb) $ \dptr ->
      copyBytes (dptr `plusPtr` (slice + doffset)) (sptr `plusPtr` soffset) len
{-# INLINE putByteStringAt #-}

putLazyByteStringAt :: ByteBuffer -> Int -> LBS.ByteString -> IO ()
putLazyByteStringAt bb doffset bs = do
  let Slice slice = bbSlice bb
  let len = LBS.length bs
  boundCheck bb doffset (fromIntegral len)
  withForeignPtr (bbData bb) $ \dptr ->
    copyLazyBytes (dptr `plusPtr` (slice + doffset)) bs
  where
    copyLazyBytes _dptr LBS.Empty                              = return ()
    copyLazyBytes dptr (LBS.Chunk (BS.PS fptr soffset len) cs) = do
      withForeignPtr fptr $ \sptr -> do
        copyBytes dptr (sptr `plusPtr` soffset) len
        copyLazyBytes (dptr `plusPtr` len) cs
{-# INLINE putLazyByteStringAt #-}

{-
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
-}

getByteStringAt :: ByteBuffer -> Int -> Int -> IO BS.ByteString
getByteStringAt bb offset len = do
  boundCheck bb offset len -- XXX???
  let Slice slice = bbSlice bb
  withForeignPtr (bbData bb) $ \sptr ->
    BS.create len $ \dptr ->
      copyBytes dptr (sptr `plusPtr` (slice + offset)) len
{-# INLINE getByteStringAt #-}

unsafeGetByteStringAt :: ByteBuffer -> Int -> Int -> IO BS.ByteString
unsafeGetByteStringAt bb offset len = do
  boundCheck bb offset len -- XXX???
  let Slice slice = bbSlice bb
  withForeignPtr (bbData bb) $ \sptr ->
    BS.unsafePackCStringLen (castPtr (sptr `plusPtr` (slice + offset)), len)
{-# INLINE unsafeGetByteStringAt #-}

unsafeGetLazyByteStringAt :: ByteBuffer -> Int -> Int -> IO LBS.ByteString
unsafeGetLazyByteStringAt bb offset len =
  fmap (LBS.fromStrict) (unsafeGetByteStringAt bb offset len)
{-# INLINE unsafeGetLazyByteStringAt #-}

------------------------------------------------------------------------
-- * Absolute operations on `Storable` elements

putStorableAt :: forall a. Storable a => ByteBuffer -> Int -> a -> IO ()
putStorableAt bb ix x = do
  boundCheck bb ix (sizeOf (undefined :: a))
  withForeignPtr (bbData bb) $ \ptr ->
    pokeByteOff ptr ix x

getStorableAt :: forall a . Storable a => ByteBuffer -> Int -> IO a
getStorableAt bb ix = do
  boundCheck bb ix (sizeOf (undefined :: a))
  withForeignPtr (bbData bb) $ \ptr ->
    peekByteOff ptr ix

------------------------------------------------------------------------

primitiveInt :: (Addr# -> Int# -> State# RealWorld -> (# State# RealWorld, Int# #))
             -> (Int# -> i) -> Int -> ByteBuffer -> Int -> IO i
primitiveInt f c size bb offset@(I# offset#) = do
  boundCheck bb offset size
  let Slice (I# slice#) = bbSlice bb
  withForeignPtr (bbData bb) $ \(Ptr addr#) ->
    IO $ \s ->
      case f (addr# `plusAddr#` (offset# +# slice#)) 0# s of
        (# s', i #) -> (# s', c i #)
{-# INLINE primitiveInt #-}

primitiveInt32 :: (Addr# -> Int# -> State# RealWorld -> (# State# RealWorld, Int# #))
               -> ByteBuffer -> Int ->  IO Int32
primitiveInt32 f bb offset = primitiveInt f I32# (sizeOf (4 :: Int32)) bb offset
{-# INLINE primitiveInt32 #-}

primitiveInt64 :: (Addr# -> Int# -> State# RealWorld -> (# State# RealWorld, Int# #))
               -> ByteBuffer -> Int ->  IO Int64
primitiveInt64 f bb offset = primitiveInt f I64# (sizeOf (8 :: Int64)) bb offset
{-# INLINE primitiveInt64 #-}

primitiveInt_ :: (Addr# -> Int# -> Int# -> State# RealWorld -> State# RealWorld)
              -> (i -> Int#) -> Int -> ByteBuffer -> Int -> i -> IO ()
primitiveInt_ f d size bb offset@(I# offset#) i = do
  boundCheck bb offset size
  let value# = d i
  let Slice (I# slice#) = bbSlice bb
  withForeignPtr (bbData bb) $ \(Ptr addr#) ->
    IO $ \s ->
      case f (addr# `plusAddr#` (offset# +# slice#)) 0# value# s of
        s' -> (# s', () #)
{-# INLINE primitiveInt_ #-}

primitiveInt32_ :: (Addr# -> Int# -> Int# -> State# RealWorld -> State# RealWorld)
              -> ByteBuffer -> Int -> Int32 -> IO ()
primitiveInt32_ f = primitiveInt_ f (\(I32# x#) -> x#) (sizeOf (4 :: Int32))
{-# INLINE primitiveInt32_ #-}

primitiveInt64_ :: (Addr# -> Int# -> Int# -> State# RealWorld -> State# RealWorld)
              -> ByteBuffer -> Int -> Int64 -> IO ()
primitiveInt64_ f = primitiveInt_ f (\(I64# x#) -> x#) (sizeOf (8 :: Int64))
{-# INLINE primitiveInt64_ #-}

-- readCharOffArray#
-- readWideCharOffArray#
readIntOffArrayIx :: ByteBuffer -> Int -> IO Int
readIntOffArrayIx = primitiveInt readIntOffAddr# I# (sizeOf (8 :: Int))

-- readWordOffArray#
-- readArrayOffAddr#
-- readFloatOffArray#
-- readDoubleOffArray#
-- readStablePtrOffArray#
-- readInt8OffArray#
-- readInt16OffArray#

readInt32OffAddr :: ByteBuffer -> Int -> IO Int32
readInt32OffAddr = primitiveInt32 readInt32OffAddr#

readInt64OffAddr :: ByteBuffer -> Int -> IO Int64
readInt64OffAddr = primitiveInt64 readInt64OffAddr#

indexWord8OffAddr :: ByteBuffer -> Int -> IO Word8
indexWord8OffAddr bb offset@(I# offset#) = do
  boundCheck bb offset (sizeOf (1 :: Word8))
  withForeignPtr (bbData bb) $ \(Ptr addr#) ->
    return (fromIntegral (W# (indexWord8OffAddr# addr# offset#)))

primitiveWord :: (Addr# -> Int# -> State# RealWorld -> (# State# RealWorld, Word# #))
             -> (Word# -> w) -> Int -> ByteBuffer -> Int -> IO w
primitiveWord f c size bb offset@(I# offset#) = do
  boundCheck bb offset size
  let Slice (I# slice#) = bbSlice bb
  withForeignPtr (bbData bb) $ \(Ptr addr#) ->
    IO $ \s ->
      case f (addr# `plusAddr#` (offset# +# slice#)) 0# s of
        (# s', i #) -> (# s', c i #)
{-# INLINE primitiveWord #-}

primitiveWord_ :: (Addr# -> Int# -> Word# -> State# RealWorld -> State# RealWorld)
              -> (w -> Word#) -> Int -> ByteBuffer -> Int -> w -> IO ()
primitiveWord_ f d size bb offset@(I# offset#) w = do
  boundCheck bb offset size
  let value# = d w
  let Slice (I# slice#) = bbSlice bb
  withForeignPtr (bbData bb) $ \(Ptr addr#) ->
    IO $ \s ->
      case f (addr# `plusAddr#` (offset# +# slice#)) 0# value# s of
        s' -> (# s', () #)
{-# INLINE primitiveWord_ #-}

readWord8OffAddr :: ByteBuffer -> Int -> IO Word8
readWord8OffAddr = primitiveWord readWord8OffAddr# W8# (sizeOf (1 :: Word8))

readWord16OffAddr :: ByteBuffer -> Int -> IO Word16
readWord16OffAddr = primitiveWord readWord16OffAddr# W16# (sizeOf (2 :: Word16))

readWord32OffAddr :: ByteBuffer -> Int -> IO Word32
readWord32OffAddr = primitiveWord readWord32OffAddr# W32# (sizeOf (4 :: Word32))

readWord64OffAddr :: ByteBuffer -> Int -> IO Word64
readWord64OffAddr = primitiveWord readWord64OffAddr# W64# (sizeOf (8 :: Word64))

  {-
-- writeCharOffArray#
-- writeWideCharOffArray#
-}
writeInt = writeIntOffAddr

writeIntOffAddr :: ByteBuffer -> Int -> Int -> IO ()
writeIntOffAddr = primitiveInt_ writeIntOffAddr# (\(I# x#) -> x#) (sizeOf (8 :: Int))

-- writeWordOffArray#
-- writeArrayOffAddr#
-- writeFloatOffArray#
-- writeDoubleOffArray#
-- writeStablePtrOffArray#
-- writeInt8OffArray#
-- writeInt16OffArray#

writeInt32OffAddr :: ByteBuffer -> Int -> Int32 -> IO ()
writeInt32OffAddr = primitiveInt32_ writeInt32OffAddr#

writeInt64OffAddr :: ByteBuffer -> Int -> Int64 -> IO ()
writeInt64OffAddr = primitiveInt64_ writeInt64OffAddr#

writeWord8OffAddr :: ByteBuffer -> Int -> Word8 -> IO ()
writeWord8OffAddr = primitiveWord_ writeWord8OffAddr# (\(W8# w#) -> w#) (sizeOf (1 :: Word8))

writeWord16OffAddr :: ByteBuffer -> Int -> Word16 -> IO ()
writeWord16OffAddr = primitiveWord_ writeWord16OffAddr# (\(W16# w#) -> w#) (sizeOf (2 :: Word16))

writeWord32OffAddr :: ByteBuffer -> Int -> Word32 -> IO ()
writeWord32OffAddr = primitiveWord_ writeWord32OffAddr# (\(W32# w#) -> w#) (sizeOf (4 :: Word32))

writeWord64OffAddr :: ByteBuffer -> Int -> Word64 -> IO ()
writeWord64OffAddr = primitiveWord_ writeWord64OffAddr# (\(W64# w#) -> w#) (sizeOf (8 :: Word64))

-- atomicReadIntArray#
-- atomicWriteIntArray#

-- | Given a bytebuffer, an offset in machine words, the expected old value, and
-- the new value, perform an atomic compare and swap i.e. write the new value if
-- the current value matches the provided old value. Returns a boolean
-- indicating whether the compare and swap succeded or not. Implies a full
-- memory barrier.
casInt32Addr :: ByteBuffer -> Int -> Int32 -> Int32 -> IO Bool
casInt32Addr bb offset expected desired = do
  boundCheck bb offset (sizeOf (4 :: Int32))
  let Slice slice = bbSlice bb
  withForeignPtr (bbData bb) $ \ptr ->
    casInt32Ptr (ptr `plusPtr` (offset + slice)) expected desired

casInt64Addr :: ByteBuffer -> Int -> Int64 -> Int64 -> IO Bool
casInt64Addr bb offset expected desired = do
  boundCheck bb offset (sizeOf (8 :: Int64))
  let Slice slice = bbSlice bb
  withForeignPtr (bbData bb) $ \ptr ->
    casInt64Ptr (ptr `plusPtr` (offset + slice)) expected desired

casIntAddr :: ByteBuffer -> Int -> Int -> Int -> IO Bool
casIntAddr bb offset expected desired = do
  boundCheck bb offset (sizeOf (8 :: Int))
  let Slice slice = bbSlice bb
  withForeignPtr (bbData bb) $ \ptr ->
    casIntPtr (ptr `plusPtr` (offset + slice)) expected desired

-- | Given a bytebuffer, and offset in machine words, and a value to add,
-- atomically add the value to the element. Returns the value of the element
-- before the operation. Implies a full memory barrier.
fetchAddInt64Array :: ByteBuffer -> Int -> Int64 -> IO Int64
fetchAddInt64Array bb offset incr = do
  boundCheck bb offset (sizeOf (8 :: Int64))
  let Slice slice = bbSlice bb
  withForeignPtr (bbData bb) $ \ptr ->
    fromIntegral <$> fetchAddInt64Ptr (ptr `plusPtr` (offset + slice)) incr
{-# INLINE fetchAddInt64Array #-}

-- | Given a bytebuffer, and offset in machine words, and a value to add,
-- atomically add the value to the element. Implies a full memory barrier.
fetchAddInt64Array_ :: ByteBuffer -> Int -> Int64 -> IO ()
fetchAddInt64Array_ bb offset incr = do
  boundCheck bb offset (sizeOf (8 :: Int64))
  let Slice slice = bbSlice bb
  withForeignPtr (bbData bb) $ \ptr ->
    void $ fetchAddInt64Ptr (ptr `plusPtr` (offset + slice)) incr
{-# INLINE fetchAddInt64Array_ #-}

fetchAddIntArray_ :: ByteBuffer -> Int -> Int -> IO ()
fetchAddIntArray_ bb offset (I# incr) = fetchAddInt64Array_ bb offset (I64# incr)
{-# INLINE fetchAddIntArray_ #-}

{-
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
  withForeignPtr (bbData bb) $ \ptr ->
    msync ptr (fromIntegral (bbCapacity bb)) mS_SYNC False
{-# INLINE force #-}

forceAt :: ByteBuffer -> Int -> Int -> IO ()
forceAt bb offset len = do
  let Slice slice = bbSlice bb
  withForeignPtr (bbData bb) $ \ptr ->
    msync (ptr `plusPtr` (slice + offset)) (fromIntegral len) mS_SYNC False
{-# INLINE forceAt #-}
