module Journal.Internal.ByteBufferTest where

import Control.Arrow ((&&&))
import Control.Exception (IOException, catch, displayException)
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
import System.Directory (removePathForcibly, canonicalizePath, getTemporaryDirectory)
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
prettyRunLenEnc []       = ""
prettyRunLenEnc [(n, w)] = show n ++ "x" ++ show (w2c w)
prettyRunLenEnc ((n, w) : nws) =
  show n ++ "x" ++ show (w2c w) ++ ", " ++ prettyRunLenEnc nws

data Command
  = ReadInt32 Int
  | WriteInt32 Int Int32
  deriving Show

newFakeByteBuffer :: Int -> FakeByteBuffer
newFakeByteBuffer size = FakeByteBuffer
  { fbbVector = Vector.replicate size 0
  , fbbSize   = size
  }

readInt32Fake :: FakeByteBuffer -> Int -> (FakeByteBuffer, Int32)
readInt32Fake fbb offset =
  let
    bytes :: [Word8]
    bytes = Vector.toList
          $ Vector.take (sizeOf (4 :: Int32))
          $ Vector.drop offset (fbbVector fbb)
  in
    (fbb, decode (LBS.pack bytes))

writeInt32Fake :: FakeByteBuffer -> Int -> Int32 -> (FakeByteBuffer, ())
writeInt32Fake fbb offset value =
  let
    bytes :: [Word8]
    bytes = LBS.unpack (encode value)

    indexValues :: [(Int, Word8)]
    indexValues = zip [offset .. offset + length bytes - 1] bytes
  in
    (fbb { fbbVector = fbbVector fbb Vector.// indexValues }, ())

------------------------------------------------------------------------

constructorString :: Command -> String
constructorString ReadInt32  {} = "ReadInt32"
constructorString WriteInt32 {} = "WriteInt32"

prettyCommand :: Command -> String
prettyCommand = show

data Response
  = Unit ()
  | Int32 Int32
  | IOException IOException
  deriving Eq

prettyResponse :: Response -> String
prettyResponse (Unit ())       = "Unit ()"
prettyResponse (Int32 i)       = "Int32 " ++ show i
prettyResponse (IOException e) = "IOException " ++ displayException e

type Model = FakeByteBuffer

precondition :: Model -> Command -> Bool
precondition _m _cmd = True

step :: Command -> Model -> (Model, Response)
step (ReadInt32 offset)        m = Int32 <$> readInt32Fake  m offset
step (WriteInt32 offset value) m = Unit  <$> writeInt32Fake m offset value

exec :: Command -> ByteBuffer -> IO Response
exec (ReadInt32  offset)       bb =
  Int32 <$> readInt32OffAddr bb (roundupBytesToSizeOfElem offset (sizeOf (4 :: Int32)))
exec (WriteInt32 offset value) bb =
  Unit <$> writeInt32OffAddr bb (roundupBytesToSizeOfElem offset (sizeOf (4 :: Int32))) value

roundupBytesToSizeOfElem :: Int -> Int -> Int
roundupBytesToSizeOfElem n size = ((n + size) - 1) `div` size

genCommand :: Model -> Gen Command
genCommand m = frequency
  [ (1, ReadInt32  <$> genOffset (sizeOf (4 :: Int32)) (fbbSize m))
  , (1, WriteInt32 <$> genOffset (sizeOf (4 :: Int32)) (fbbSize m) <*> arbitrary)
  ]

genOffset :: Int -> Int -> Gen Int
genOffset sizeOfElem sizeOfVec =
  elements [ o | o <- [ 0 .. (sizeOfVec `div` sizeOfElem) - 1 ] ]

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
        resp' <- run (exec cmd bb `catch` (return . IOException))
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
      resp' <- exec cmd bb `catch` (return . IOException)
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

unit_bbBug0 :: Assertion
unit_bbBug0 = assertProgram ""
  -- [WriteInt32 57 1,ReadInt32 58]
  [ReadInt32 524]

-- XXX: Make MmapTest pass first.
  {-
nit_byteBufferMmapped :: Assertion
nit_byteBufferMmapped = do
  pageSize <- sysconfPageSize
  withTempFile tmp "" $ \fp _handle -> do
    fallocate fp pageSize
    bb <- mmapped fp pageSize
    -- bb <- allocateAligned pageSize pageSize

    let ix = 1344

    let i32 :: Int32
        i32 = minBound

    writeInt32OffAddr bb ix i32
    j32 <- readInt32OffAddr bb ix
    assertEqual "" i32 j32

    let i32' :: Int32
        i32' = maxBound

    writeInt32OffAddr bb ix i32'
    j32' <- readInt32OffAddr bb ix
    assertEqual "" i32' j32'

    let i64 :: Int64
        i64 = minBound

    writeInt64OffAddr bb ix i64
    j64 <- readInt64OffAddr bb ix
    assertEqual "" i64 j64

    let i64' :: Int64
        i64' = maxBound

    writeInt64OffAddr bb ix i64'
    j64' <- readInt64OffAddr bb ix
    assertEqual "" i64' j64'
-}

------------------------------------------------------------------------

assertProgram :: String -> [Command] -> Assertion
assertProgram msg cmds = do
  b <- runCommands cmds
  assertBool msg b