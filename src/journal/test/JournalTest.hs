{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveFunctor #-}

module JournalTest where

import Control.Arrow ((&&&))
import Control.Exception (IOException, catch, displayException)
import Control.Monad (unless, when)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.ByteString.Internal (w2c)
import Data.Monoid (Sum(Sum))
import Data.Vector (Vector)
import qualified Data.Vector as Vector
import Data.Word (Word8)
import System.Directory (removePathForcibly)
import System.Timeout (timeout)
import Test.QuickCheck
import Test.QuickCheck.Instances.ByteString ()
import Test.QuickCheck.Monadic
import Test.Tasty.HUnit (Assertion, assertBool)

import Journal
import Journal.Internal

------------------------------------------------------------------------

data FakeJournal' a = FakeJournal
  { fjJournal :: Vector a
  , fjIndex   :: Int
  }
  deriving (Show, Functor)

type FakeJournal = FakeJournal' ByteString

prettyFakeJournal :: FakeJournal -> String
prettyFakeJournal = show . fmap (prettyRunLenEnc . runLengthEncoding)

startJournalFake :: FakeJournal
startJournalFake = FakeJournal Vector.empty 0

appendBSFake :: ByteString -> FakeJournal -> (FakeJournal, ())
appendBSFake bs (FakeJournal jour ix) = (FakeJournal (Vector.snoc jour bs) ix, ())

readJournalFake :: FakeJournal -> (FakeJournal, ByteString)
readJournalFake fj@(FakeJournal jour ix) = (FakeJournal jour (ix + 1), jour Vector.! ix)

------------------------------------------------------------------------

data Command
  = AppendBS ByteString
  -- Tee
  -- AppendRecv
  | ReadJournal
  -- SaveSnapshot
  -- TruncateAfterSnapshot
  -- LoadSnapshot
  -- Replay
  deriving Show

constructorString :: Command -> String
constructorString AppendBS {} = "AppendBS"
constructorString ReadJournal = "ReadJournal"

prettyCommand :: Command -> String
prettyCommand (AppendBS bs) =
  "AppendBS \"" ++ prettyRunLenEnc (runLengthEncoding bs) ++ "\""
prettyCommand ReadJournal   = "ReadJournal"

runLengthEncoding :: ByteString -> [(Int, Word8)]
runLengthEncoding = map (BS.length &&& BS.head) . BS.group

prettyRunLenEnc :: [(Int, Word8)] -> String
prettyRunLenEnc [(n, w)] = show n ++ "x" ++ [ w2c w ]

data Response
  = Unit ()
  | ByteString ByteString
  | IOException IOException
  deriving Eq

prettyResponse :: Response -> String
prettyResponse (Unit ())       = "Unit ()"
prettyResponse (ByteString bs) =
  "ByteString \"" ++ prettyRunLenEnc (runLengthEncoding bs) ++ "\""
prettyResponse (IOException e) = "IOException " ++ displayException e

type Model = FakeJournal

-- If there's nothing new to read, then don't generate reads (because they are
-- blocking) and don't append empty bytestrings.
precondition :: Model -> Command -> Bool
precondition m ReadJournal   = Vector.length (fjJournal m) /= fjIndex m
precondition m (AppendBS bs) =
  not (BS.null bs) && BS.length bs + hEADER_SIZE <= oMaxByteSize testOptions

step :: Command -> Model -> (Model, Response)
step (AppendBS bs) m = Unit <$> appendBSFake bs m
step ReadJournal   m = ByteString <$> readJournalFake m

exec :: Command -> (Journal, JournalConsumer) -> IO Response
exec (AppendBS bs) (j, _jc) = Unit <$> appendBS j bs
exec ReadJournal   (_j, jc) = ByteString <$> readJournal jc

-- Generates ASCII bytestrings.
genByteString :: Gen ByteString
genByteString = oneof (map genBs [65..90])
  where
    genBs :: Int -> Gen ByteString
    genBs i = frequency
      [ (1, sized (\n -> return (BS.replicate n (fromIntegral i))))
      , (1, return (BS.replicate  maxLen      (fromIntegral i)))
      , (1, return (BS.replicate (maxLen - 1) (fromIntegral i)))
      ]
    maxLen = oMaxByteSize testOptions - hEADER_SIZE - fOOTER_SIZE

genCommand :: Gen Command
genCommand = frequency
  [ (1, AppendBS <$> genByteString)
  , (1, pure ReadJournal)
  ]

genCommands :: Model -> Gen [Command]
genCommands m0 = sized (go m0)
  where
    go :: Model -> Int -> Gen [Command]
    go _m 0 = return []
    go m  n = do
      cmd <- genCommand `suchThat` precondition m
      cmds <- go (fst (step cmd m)) (n - 1)
      return (cmd : cmds)

shrinkCommand :: Command -> [Command]
shrinkCommand ReadJournal   = []
shrinkCommand (AppendBS bs) =
  [ AppendBS (BS.pack w8s)
  | w8s <- shrinkList (const []) (BS.unpack bs)
  , not (null w8s) ]

shrinkCommands :: Model -> [Command] -> [[Command]]
shrinkCommands m = filter (validProgram m) . shrinkList shrinkCommand

validProgram :: Model -> [Command] -> Bool
validProgram = go True
  where
    go False _m _cmds       = False
    go valid _m []          = valid
    go valid m (cmd : cmds) = go (precondition m cmd) (fst (step cmd m)) cmds

tEST_DIRECTORY :: FilePath
tEST_DIRECTORY = "/tmp/journal-test"

testOptions :: Options
testOptions = defaultOptions { oMaxByteSize = 128 }

prop_journal :: Property
prop_journal =
  let m = startJournalFake in
  forAllShrink (genCommands m) (shrinkCommands m) $ \cmds -> monadicIO $ do
    run (putStrLn ("Generated commands: " ++ show cmds))
    run (removePathForcibly tEST_DIRECTORY)
    jjc <- run (startJournal tEST_DIRECTORY testOptions)
    monitor (tabulate "Commands" (map constructorString cmds))
    (result, hist) <- go cmds m jjc []
    run (uncurry stopJournal jjc)
    monitorStats (stats (zip cmds hist))
    return result
    where
      go []          _m _jjc hist = return (True, reverse hist)
      go (cmd : cmds) m  jjc hist = do
        let (m', resp) = step cmd m
        resp' <- run (exec cmd jjc `catch` (return . IOException))
        assertWithFail (resp == resp') $
          prettyResponse resp ++ " /= " ++ prettyResponse resp'
        go cmds m' jjc (resp : hist)

      assertWithFail :: Monad m => Bool -> String -> PropertyM m ()
      assertWithFail condition msg = do
        unless condition $
          monitor (counterexample ("Failed: " ++ msg))
        assert condition

-- XXX: Get this straight from the metrics of the journal instead?
data Stats = Stats
  { sBytesWritten :: Int
  , sRotations    :: Int
  }
  deriving Show

stats :: [(Command, Response)] -> Stats
stats hist = Stats
  { sBytesWritten = totalAppended
  -- XXX: doesn't account for footers...
  , sRotations    = totalAppended `div` oMaxByteSize testOptions
  }
  where
    Sum totalAppended =
      foldMap (\(cmd, _resp) ->
                 case cmd of
                   AppendBS bs -> Sum (hEADER_SIZE + BS.length bs)
                   _otherwise  -> mempty) hist

monitorStats :: Monad m => Stats -> PropertyM m ()
monitorStats stats
  = monitor
  $ collect ("Bytes written: " <> show (sBytesWritten stats))
  . collect ("Rotations: "     <> show (sRotations stats))

runCommands :: [Command] -> IO Bool
runCommands cmds = do
  let m = startJournalFake
  removePathForcibly tEST_DIRECTORY
  jjc <- startJournal tEST_DIRECTORY testOptions
  putStrLn ""
  go m jjc cmds []
  where
    go m jjc [] _hist = putStrLn "\nSuccess!" >> return True
    go m jjc (cmd : cmds) hist = do
      let (m', resp) = step cmd m
      putStrLn (prettyFakeJournal m)
      putStrLn ""
      putStrLn ("    == " ++ prettyCommand cmd ++ " ==> " ++ prettyResponse resp)
      putStrLn ""
      if null cmds
      then putStrLn (prettyFakeJournal m')
      else return ()
      resp' <- exec cmd jjc `catch` (return . IOException)
      is <- checkForInconsistencies (fst jjc)
      if resp == resp' && null is
      then go m' jjc cmds ((cmd, resp) : hist)
      else do
        putStrLn ""
        when (resp /= resp') $
          putStrLn ("Failed: " ++ prettyResponse resp ++ " /= " ++ prettyResponse resp')
        when (not (null is)) $
          putStrLn ("Inconsistencies: " ++ inconsistenciesString is)
        putStrLn ""
        putStrLn "Journal dump:"
        dumpJournal (fst jjc)
        print (stats (reverse hist))
        return False

------------------------------------------------------------------------

-- XXX: make sure all these unit tests are part of the coverage...

unit_bug0 :: Assertion
unit_bug0 = assertProgram "read after rotation"
  [ AppendBS "AAAAAAAAAAAAAAAAA"
  , AppendBS "BBBBBBBBBBBBBBBB"
  , ReadJournal
  , ReadJournal
  , AppendBS "CCCCCCCCCCCCCCCCC"
  , ReadJournal
  , AppendBS "DDDDDDDDDDDDDDDD"
  , ReadJournal
  , AppendBS "EEEEEEEEEEEEEEEE"
  , ReadJournal
  , AppendBS "FFFFFFFFFFFFFFFF"
  , ReadJournal
  ]

unit_bug1 :: Assertion
unit_bug1 = assertProgram "two rotations"
  [ AppendBS "XXXXXXXXXXXXXXXXXXXX"
  , AppendBS "XXXXXXXXXXXXXXXXXXXX"
  , AppendBS "XXXXXXXXXXXXXXXXXXXX"
  , AppendBS "XXXXXXXXXXXXXXXXXXXX"
  , AppendBS "XXXXXXXXXXXXXXXXXXXX"
  , AppendBS "XXXXXXXXXXXXXXXXXXXX"
  , AppendBS "XXXXXXXXXXXXXXXXXXXX"
  , AppendBS "XXXXXXXXXXXXXXXXXXXX"
  , AppendBS "XXXXXXXXXXXXXXXXXXXX"
  , AppendBS "XXXXXXXXXXXXXXXXXXXX"
  ]

unit_bug2 :: Assertion
unit_bug2 = assertProgram "stuck reading"
  [ AppendBS "OOOOOOOOOOOOO"
  , ReadJournal
  , AppendBS "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA" -- 116 + 6 = 122 bytes
  , ReadJournal
  , AppendBS "UUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUU" -- 116 + 6 = 122 bytes
  , ReadJournal
  ]

unit_bug3 :: Assertion
unit_bug3 = assertProgram "two rotations reading side"
  [ AppendBS "M"
  , AppendBS "RRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRR" -- 110 + 6 = 116 bytes
  , AppendBS "L"
  , ReadJournal
  , ReadJournal
  ]

------------------------------------------------------------------------

assertProgram :: String -> [Command] -> Assertion
assertProgram msg cmds = do
  b <- runCommands cmds
  assertBool msg b
