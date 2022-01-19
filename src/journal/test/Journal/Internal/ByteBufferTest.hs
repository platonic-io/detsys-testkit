{-# LANGUAGE ScopedTypeVariables #-}

module Journal.Internal.ByteBufferTest where

import Control.Arrow ((&&&))
import Control.Exception
       ( ArrayException(IndexOutOfBounds)
       , Handler(Handler)
       , IOException
       , catches
       , displayException
       )
import Control.Monad (unless, when)
import Data.Binary (decode, encode)
import Data.ByteString.Internal (w2c)
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Char8 as BSChar8
import qualified Data.ByteString.Lazy as LBS
import Data.Int (Int32, Int64)
import Data.List (group)
import Data.Vector (Vector)
import qualified Data.Vector as Vector
import Data.Word (Word8)
import Foreign (sizeOf)
import GHC.ByteOrder
       (ByteOrder(BigEndian, LittleEndian), targetByteOrder)
import System.Directory
       (canonicalizePath, getTemporaryDirectory, removePathForcibly)
import System.IO (hClose, openTempFile)
import Test.QuickCheck
import Test.QuickCheck.Monadic
import Test.Tasty.HUnit (Assertion, assertBool, assertEqual)

import Journal.Internal.ByteBufferPtr
import Journal.Internal.Mmap
import Journal.Internal.Utils

------------------------------------------------------------------------

data FakeByteBuffer = FakeByteBuffer
  { fbbVector :: Vector Word8
  , fbbSize   :: Int
  }
  deriving Show

prettyFakeByteBuffer :: FakeByteBuffer -> String
prettyFakeByteBuffer fbb = concat
  [ "FakeByteBuffer "
  , "{ fbbVector = " ++ prettyRunLenEnc (runLengthEncoding (fbbVector fbb))
  , ", fbbSize = "   ++ show (fbbSize fbb)
  , "}"
  ]

runLengthEncoding :: Vector Word8 -> [(Int, Word8)]
runLengthEncoding = map (length &&& head) . group . Vector.toList

prettyRunLenEnc :: [(Int, Word8)] -> String
prettyRunLenEnc nws0 =
  case nws0 of
    []             -> ""
    [(n, w)]       -> go n w
    ((n, w) : nws) ->
      go n w ++ ", " ++ prettyRunLenEnc nws
  where
    go 1 w = show (w2c w)
    go n w = show n ++ "x" ++ show (w2c w)

data WhatBuffer = WBOriginal | WBWrapper WhatWrapper
  deriving (Eq, Show)

data WhatWrapper = WW1 | WW2
  deriving (Eq, Show)

data Index = Index
  { iOffset :: Int
  , iBuffer :: WhatBuffer
  } deriving (Eq, Show)

data Command
  = ReadInt32 Index
  | WriteInt32 Index Int32
  | ReadInt64 Index
  | WriteInt64 Index Int64
  | MkWrapPart WhatWrapper WhatBuffer Int Int
  deriving Show

newFakeByteBuffer :: Int -> FakeByteBuffer
newFakeByteBuffer size = FakeByteBuffer
  { fbbVector = Vector.replicate size 0
  , fbbSize   = size
  }

fixEndianess :: [a] -> [a]
fixEndianess = case targetByteOrder of
  LittleEndian -> reverse
  BigEndian    -> id -- Data.Binary always uses big endian.

readInt32Fake :: FakeByteBuffer -> Int -> (FakeByteBuffer, Int32)
readInt32Fake fbb offset =
  let
    bytes :: [Word8]
    bytes = fixEndianess
          $ Vector.toList
          $ Vector.take (sizeOf (4 :: Int32))
          $ Vector.drop offset (fbbVector fbb)
  in
    (fbb, decode (LBS.pack bytes))

writeInt32Fake :: FakeByteBuffer -> Int -> Int32 -> (FakeByteBuffer, ())
writeInt32Fake fbb offset value =
  let
    bytes :: [Word8]
    bytes = fixEndianess (LBS.unpack (encode value))

    indexValues :: [(Int, Word8)]
    indexValues = zip [offset .. offset + length bytes - 1] bytes
  in
    (fbb { fbbVector = fbbVector fbb Vector.// indexValues }, ())

readInt64Fake :: FakeByteBuffer -> Int -> (FakeByteBuffer, Int64)
readInt64Fake fbb offset =
  let
    bytes :: [Word8]
    bytes = fixEndianess
          $ Vector.toList
          $ Vector.take (sizeOf (8 :: Int64))
          $ Vector.drop offset (fbbVector fbb)
  in
    (fbb, decode (LBS.pack bytes))

writeInt64Fake :: FakeByteBuffer -> Int -> Int64 -> (FakeByteBuffer, ())
writeInt64Fake fbb offset value =
  let
    bytes :: [Word8]
    bytes = fixEndianess (LBS.unpack (encode value))

    indexValues :: [(Int, Word8)]
    indexValues = zip [offset .. offset + length bytes - 1] bytes
  in
    (fbb { fbbVector = fbbVector fbb Vector.// indexValues }, ())

------------------------------------------------------------------------

constructorString :: Command -> String
constructorString ReadInt32  {} = "ReadInt32"
constructorString WriteInt32 {} = "WriteInt32"
constructorString ReadInt64  {} = "ReadInt64"
constructorString WriteInt64 {} = "WriteInt64"
constructorString MkWrapPart {} = "MkWrapPart"

prettyCommand :: Command -> String
prettyCommand = show

data Response
  = Unit ()
  | Int32 Int32
  | Int64 Int64
  | IOException IOException
  | IndexOutOfBound
  deriving Eq

prettyResponse :: Response -> String
prettyResponse (Unit ())       = "Unit ()"
prettyResponse (Int32 i)       = "Int32 " ++ show i
prettyResponse (Int64 i)       = "Int64 " ++ show i
prettyResponse (IOException e) = "IOException " ++ displayException e
prettyResponse IndexOutOfBound = "IndexOutOfBound"

data FakeWrapper = FakeWrapper
  { fwSlice :: Int
  , fwLimit :: Int
  }
  deriving Show

data Model = Model
  { mBuffer :: FakeByteBuffer
  , mWrapper1 :: FakeWrapper
  , mWrapper2 :: FakeWrapper
  }

prettyModel :: Model -> String
prettyModel m = unlines
  [ "Model "
  , "  { mBuffer = " ++ prettyFakeByteBuffer (mBuffer m)
  , "  , mWrapper1 = " ++ show (mWrapper1 m)
  , "  , mWrapper2 = " ++ show (mWrapper2 m)
  , "  }"
  ]

newModel :: Int -> Model
newModel size = Model
  { mBuffer = newFakeByteBuffer size
  , mWrapper1 = FakeWrapper 0 size
  , mWrapper2 = FakeWrapper 0 size
  }

precondition :: Model -> Command -> Bool
precondition _m _cmd = True

validRange :: (Int, Int) -> Bool
validRange (a, b) = a <= b

inRange :: (Int, Int) -> (Int, Int) -> Bool
(a,b) `inRange` (c, d) = c <= a && b <= d
  -- These should hold for it to be a range
  && validRange (a,b)
  && validRange (c,d)

fullRange :: Model -> WhatBuffer -> (Int, Int)
fullRange m mode = case mode of
  WBOriginal -> (0, fbbSize (mBuffer m))
  (WBWrapper WW1) -> wrap (mWrapper1 m)
  (WBWrapper WW2) -> wrap (mWrapper2 m)
  where
    wrap fw = (lower fw, upper fw)
    lower fw = fwSlice fw
    upper fw = lower fw + fwLimit fw

validOffset :: Index -> Int -> Model -> Bool
validOffset i@(Index offset mode) size m =
   mkRange offset' `inRange` exl (fullRange m mode) &&
   mkRange offset  `inRange` exl (0, limit)
  where
    exl (a, b) = (a, b-1)
    offset' = realOffset m i
    mkRange o = (o, o + size - 1)
    limit = case mode of
      WBOriginal -> fbbSize (mBuffer m)
      WBWrapper w -> fwLimit $ getWrapper m w

getWrapper :: Model -> WhatWrapper -> FakeWrapper
getWrapper m WW1 = mWrapper1 m
getWrapper m WW2 = mWrapper2 m

realOffset :: Model -> Index -> Int
realOffset _ (Index offset WBOriginal) = offset
realOffset m (Index offset (WBWrapper w)) = offset + fwSlice (getWrapper m w)

step :: Command -> Model -> (Model, Response)
step cmd m = case cmd of
  ReadInt32 offset        -> checkValid offset 4 $ \o ->
    Int32 <$> readInt32Fake  (mBuffer m) o
  WriteInt32 offset value -> checkValid offset 4 $ \o ->
    Unit  <$> writeInt32Fake (mBuffer m) o value
  ReadInt64 offset        -> checkValid offset 8 $ \o ->
    Int64 <$> readInt64Fake  (mBuffer m) o
  WriteInt64 offset value -> checkValid offset 8 $ \o ->
    Unit  <$> writeInt64Fake (mBuffer m) o value
  MkWrapPart WW1 wb offset len -> checkValidWrap wb offset len $ \fw -> m {mWrapper1 = fw }
  MkWrapPart WW2 wb offset len -> checkValidWrap wb offset len $ \fw -> m {mWrapper2 = fw }
  where
    startOffset WBOriginal = 0
    startOffset (WBWrapper w) = fwSlice $ getWrapper m w

    checkValidWrap :: WhatBuffer -> Int -> Int -> (FakeWrapper -> Model) -> (Model, Response)
    checkValidWrap wb offset len f
      | (startOffset wb + offset, startOffset wb + offset+len) `inRange` fullRange m wb = (f $ FakeWrapper (offset + startOffset wb) len, Unit ())
      | otherwise = (m, IndexOutOfBound)

    checkValid offset size f
      | validOffset offset size m
      , let (b', r) = f (realOffset m offset) = (m{mBuffer = b'}, r)
      | otherwise = (m, IndexOutOfBound)

data ByteBuffers = ByteBuffers
 { bbOriginal :: ByteBuffer
 , bbWrapper1 :: ByteBuffer
 , bbWrapper2 :: ByteBuffer
 }

selectByteBuffer :: ByteBuffers -> WhatBuffer -> ByteBuffer
selectByteBuffer bbs WBOriginal = bbOriginal bbs
selectByteBuffer bbs (WBWrapper WW1) = bbWrapper1 bbs
selectByteBuffer bbs (WBWrapper WW2) = bbWrapper2 bbs

exec' :: Command -> ByteBuffers -> IO (Response, ByteBuffers)
exec' cmd  bbs = case cmd of
  ReadInt32  offset -> run Int32 readInt32OffAddr offset
  WriteInt32 offset value -> runValue writeInt32OffAddr offset value
  ReadInt64  offset -> run Int64 readInt64OffAddr offset
  WriteInt64 offset value -> runValue writeInt64OffAddr offset value
  MkWrapPart lv rv offset len -> do
    bb' <- wrapPart (selectByteBuffer bbs rv) offset len
    case lv of
      WW1 -> return (Unit (), bbs { bbWrapper1 = bb'})
      WW2 -> return (Unit (), bbs { bbWrapper2 = bb'})
  where
    run :: (a -> Response) -> (ByteBuffer -> Int -> IO a) -> Index -> IO (Response, ByteBuffers)
    run wrapper action offset = do
      result <- action (selectByteBuffer bbs (iBuffer offset)) (iOffset offset)
      return (wrapper result, bbs)

    runValue :: (ByteBuffer -> Int -> v -> IO ()) -> Index -> v -> IO (Response, ByteBuffers)
    runValue action offset value = do
      result <- action (selectByteBuffer bbs (iBuffer offset)) (iOffset offset) value
      return (Unit result, bbs)

exec :: Command -> ByteBuffers -> IO (Response, ByteBuffers)
exec c b = exec' c b
  `catches` [ Handler (\(IndexOutOfBounds {}) -> return (IndexOutOfBound, b))
            , Handler (\(ex :: IOException)   -> return (IOException ex, b))
            ]

genCommand :: Model -> Gen Command
genCommand m = do
  wb <- elements [WBOriginal, WBWrapper WW1, WBWrapper WW2]
  frequency
    [ (1, ReadInt32  <$> genOffset wb (sizeOf (4 :: Int32)) sizeOfVec)
    , (1, WriteInt32 <$> genOffset wb (sizeOf (4 :: Int32)) sizeOfVec <*> arbitrary)
    , (1, ReadInt64  <$> genOffset wb (sizeOf (8 :: Int64)) sizeOfVec)
    , (1, WriteInt64 <$> genOffset wb (sizeOf (8 :: Int64)) sizeOfVec <*> arbitrary)
    , (1, MkWrapPart <$> elements [WW1, WW2] <*> pure wb <*> arbitrary <*> arbitrary)
    ]
  where
    sizeOfVec = fbbSize $ mBuffer m

genOffset :: WhatBuffer -> Int -> Int -> Gen Index
genOffset wb sizeOfElem sizeOfVec = fmap (\i -> Index i wb) $
  frequency
    [ (10, chooseInt (0, sizeOfVec - sizeOfElem - 1)) -- valid-index
    , (1, chooseInt (-sizeOfVec, -1)) -- invalid before
    , (3, chooseInt (sizeOfVec - sizeOfElem, sizeOfVec - sizeOfElem + 8)) -- invalid after
    , (1, chooseInt (sizeOfVec - sizeOfElem + 8, 3 * sizeOfVec)) -- invalid after
    ]

genCommands :: Model -> Gen [Command]
genCommands m0 = sized (go m0)
  where
    go :: Model -> Int -> Gen [Command]
    go _m 0 = return []
    go m  n = do
      cmd <- genCommand m `suchThat` precondition m
      cmds <- go (fst (step cmd m)) (n - 1)
      return (cmd : cmds)

shrinkCommand :: Command -> [Command]
shrinkCommand ReadInt32 {} = []
shrinkCommand (WriteInt32 offset value) =
  [ WriteInt32 offset value'
  | value' <- shrink value
  ]
shrinkCommand ReadInt64 {} = []
shrinkCommand (WriteInt64 offset value) =
  [ WriteInt64 offset value'
  | value' <- shrink value
  ]
shrinkCommand (MkWrapPart lv rv o l) =
  [ MkWrapPart lv rv o' l'
  | (o', l') <- shrink (o, l)
  ]

shrinkCommands :: Model -> [Command] -> [[Command]]
shrinkCommands m = filter (validProgram m) . shrinkList shrinkCommand

validProgram :: Model -> [Command] -> Bool
validProgram = go True
  where
    go False _m _cmds       = False
    go valid _m []          = valid
    go valid m (cmd : cmds) = go (precondition m cmd) (fst (step cmd m)) cmds

makeInitialByteBuffers :: ByteBuffer -> IO ByteBuffers
makeInitialByteBuffers bb = do
  let Capacity size = getCapacity bb
  bb1 <- wrapPart bb 0 size
  bb2 <- wrapPart bb 0 size
  return ByteBuffers
    { bbOriginal = bb
    , bbWrapper1 = bb1
    , bbWrapper2 = bb2
    }

prop_byteBuffer :: Property
prop_byteBuffer =
  let m = newModel 4096 in -- XXX: needs to be passed in?
  forAllShrink (genCommands m) (shrinkCommands m) $ \cmds -> monadicIO $ do
    run (putStrLn ("Generated commands: " ++ show cmds))
    tmp <- run (canonicalizePath =<< getTemporaryDirectory)
    (fp, h) <- run (openTempFile tmp "ByteBufferTest")
    run (print fp)
    pageSize <- run sysconfPageSize
    run (fallocate fp pageSize)
    bb <- run (mmapped fp pageSize)
    run (hClose h)
    bbs <- run (makeInitialByteBuffers bb)
    monitor (tabulate "Commands" (map constructorString cmds))
    (result, hist) <- go cmds m bbs []
    -- monitorStats (stats (zip cmds hist))
    -- XXX: removeFile fp
    return result
    where
      go []          _m _bb hist = return (True, reverse hist)
      go (cmd : cmds) m  bb hist = do
        let (m', resp) = step cmd m
        (resp', bb') <- run (exec cmd bb)
        assertWithFail (resp == resp') $
          "expected: " ++ prettyResponse resp ++ "\n        got: " ++ prettyResponse resp'
        go cmds m' bb' (resp : hist)

      assertWithFail :: Monad m => Bool -> String -> PropertyM m ()
      assertWithFail condition msg = do
        unless condition $
          monitor (counterexample ("Failed, " ++ msg))
        assert condition

runCommands :: [Command] -> IO Bool
runCommands cmds = do
  pageSize <- sysconfPageSize
  let m         = newModel pageSize
      tEST_FILE = "/tmp/hs_bytebuffertest.txt"
  removePathForcibly tEST_FILE
  fallocate tEST_FILE pageSize
  bb <- mmapped tEST_FILE pageSize
  putStrLn ""
  bbs <- makeInitialByteBuffers bb
  go m bbs cmds []
  where
    go m bb [] _hist = putStrLn "\nSuccess!" >> return True
    go m bb (cmd : cmds) hist = do
      let (m', resp) = step cmd m
      putStrLn (prettyModel m)
      putStrLn ""
      putStrLn ("    == " ++ prettyCommand cmd ++ " ==> " ++ prettyResponse resp)
      putStrLn ""
      if null cmds
      then putStrLn (prettyModel m')
      else return ()
      (resp', bb') <- exec cmd bb
      if resp == resp'
      then go m' bb' cmds ((cmd, resp) : hist)
      else do
        putStrLn ""
        when (resp /= resp') $
          putStrLn ("Failed, expected: " ++ prettyResponse resp ++ "\ngot:" ++ prettyResponse resp')
        putStrLn ""
        -- putStrLn "Bytebuffer dump:"
        -- dumpByteBuffer bb
        -- print (stats (reverse hist))
        return False

------------------------------------------------------------------------

assertProgram :: String -> [Command] -> Assertion
assertProgram msg cmds = do
  b <- runCommands cmds
  assertBool msg b

------------------------------------------------------------------------

unit_byteBufferByteString :: IO ()
unit_byteBufferByteString = do
  bb <- allocate 16
  putByteStringAt bb 0 (BSChar8.pack "helloooooooooooo")
  bs <- getByteStringAt bb 0 5
  assertEqual "" (BSChar8.pack "hello") bs
