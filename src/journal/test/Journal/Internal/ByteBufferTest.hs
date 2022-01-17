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

data Command
  = ReadInt32 Int
  | WriteInt32 Int Int32
  | ReadInt64 Int
  | WriteInt64 Int Int64
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

type Model = FakeByteBuffer

precondition :: Model -> Command -> Bool
precondition _m _cmd = True

-- should maybe have the size of the requested type?
validOffset :: Int -> Int -> Model -> Bool
validOffset offset size m
  = offset >= 0 && offset + size - 1 < fbbSize m

step :: Command -> Model -> (Model, Response)
step cmd m = case cmd of
  ReadInt32 offset        -> checkValid offset 4 $ Int32 <$> readInt32Fake  m offset
  WriteInt32 offset value -> checkValid offset 4 $ Unit  <$> writeInt32Fake m offset value
  ReadInt64 offset        -> checkValid offset 8 $ Int64 <$> readInt64Fake  m offset
  WriteInt64 offset value -> checkValid offset 8 $ Unit  <$> writeInt64Fake m offset value
  where
    checkValid offset size x
      | validOffset offset size m = x
      | otherwise = (m, IndexOutOfBound)

exec' :: Command -> ByteBuffer -> IO Response
exec' (ReadInt32  offset)       bb = Int32 <$> readInt32OffAddr bb offset
exec' (WriteInt32 offset value) bb = Unit <$> writeInt32OffAddr bb offset value
exec' (ReadInt64  offset)       bb = Int64 <$> readInt64OffAddr bb offset
exec' (WriteInt64 offset value) bb = Unit <$> writeInt64OffAddr bb offset value

exec :: Command -> ByteBuffer -> IO Response
exec c b = exec' c b
  `catches` [ Handler (\(IndexOutOfBounds {}) -> return IndexOutOfBound)
            , Handler (\(ex :: IOException)   -> return (IOException ex))
            ]

genCommand :: Model -> Gen Command
genCommand m = frequency
  [ (1, ReadInt32  <$> genOffset (sizeOf (4 :: Int32)) (fbbSize m))
  , (1, WriteInt32 <$> genOffset (sizeOf (4 :: Int32)) (fbbSize m) <*> arbitrary)
  , (1, ReadInt64  <$> genOffset (sizeOf (8 :: Int64)) (fbbSize m))
  , (1, WriteInt64 <$> genOffset (sizeOf (8 :: Int64)) (fbbSize m) <*> arbitrary)
  ]

genOffset :: Int -> Int -> Gen Int
genOffset sizeOfElem sizeOfVec =
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

shrinkCommands :: Model -> [Command] -> [[Command]]
shrinkCommands m = filter (validProgram m) . shrinkList shrinkCommand

validProgram :: Model -> [Command] -> Bool
validProgram = go True
  where
    go False _m _cmds       = False
    go valid _m []          = valid
    go valid m (cmd : cmds) = go (precondition m cmd) (fst (step cmd m)) cmds

prop_byteBuffer :: Property
prop_byteBuffer =
  let m = newFakeByteBuffer 4096 in -- XXX: needs to be passed in?
  forAllShrink (genCommands m) (shrinkCommands m) $ \cmds -> monadicIO $ do
    run (putStrLn ("Generated commands: " ++ show cmds))
    tmp <- run (canonicalizePath =<< getTemporaryDirectory)
    (fp, h) <- run (openTempFile tmp "ByteBufferTest")
    run (print fp)
    pageSize <- run sysconfPageSize
    run (fallocate fp pageSize)
    bb <- run (mmapped fp pageSize)
    run (hClose h)
    monitor (tabulate "Commands" (map constructorString cmds))
    (result, hist) <- go cmds m bb []
    -- monitorStats (stats (zip cmds hist))
    -- XXX: removeFile fp
    return result
    where
      go []          _m _bb hist = return (True, reverse hist)
      go (cmd : cmds) m  bb hist = do
        let (m', resp) = step cmd m
        resp' <- run (exec cmd bb)
        assertWithFail (resp == resp') $
          "expected: " ++ prettyResponse resp ++ "\n        got: " ++ prettyResponse resp'
        go cmds m' bb (resp : hist)

      assertWithFail :: Monad m => Bool -> String -> PropertyM m ()
      assertWithFail condition msg = do
        unless condition $
          monitor (counterexample ("Failed, " ++ msg))
        assert condition

runCommands :: [Command] -> IO Bool
runCommands cmds = do
  pageSize <- sysconfPageSize
  let m         = newFakeByteBuffer pageSize
      tEST_FILE = "/tmp/hs_bytebuffertest.txt"
  removePathForcibly tEST_FILE
  fallocate tEST_FILE pageSize
  bb <- mmapped tEST_FILE pageSize
  putStrLn ""
  go m bb cmds []
  where
    go m bb [] _hist = putStrLn "\nSuccess!" >> return True
    go m bb (cmd : cmds) hist = do
      let (m', resp) = step cmd m
      putStrLn (prettyFakeByteBuffer m)
      putStrLn ""
      putStrLn ("    == " ++ prettyCommand cmd ++ " ==> " ++ prettyResponse resp)
      putStrLn ""
      if null cmds
      then putStrLn (prettyFakeByteBuffer m')
      else return ()
      resp' <- exec cmd bb
      if resp == resp'
      then go m' bb cmds ((cmd, resp) : hist)
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
