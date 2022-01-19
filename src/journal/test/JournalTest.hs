{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE OverloadedStrings #-}

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
import System.Directory
       (canonicalizePath, getTemporaryDirectory, removeFile)
import System.IO (openTempFile)
import System.Timeout (timeout)
import Test.QuickCheck
import Test.QuickCheck.Instances.ByteString ()
import Test.QuickCheck.Monadic
import Test.Tasty.HUnit (Assertion, assertBool)

import Journal
import Journal.Internal
import Journal.Internal.Utils

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

appendBSFake :: ByteString -> FakeJournal -> (FakeJournal, Maybe ())
appendBSFake bs (FakeJournal jour ix) = (FakeJournal (Vector.snoc jour bs) ix, Just ())

readJournalFake :: FakeJournal -> (FakeJournal, Maybe ByteString)
readJournalFake fj@(FakeJournal jour ix) =
  (FakeJournal jour (ix + 1), Just (jour Vector.! ix))

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
prettyRunLenEnc nws0 = case nws0 of
  []           -> ""
  [(n, w)]     -> go n w
  (n, w) : nws -> go n w ++ " " ++ prettyRunLenEnc nws
  where
    go 1 w = [ w2c w ]
    go n w = show n ++ "x" ++ [ w2c w ]

data Response
  = Unit (Maybe ())
  | ByteString (Maybe ByteString)
  | IOException IOException
  deriving Eq

prettyResponse :: Response -> String
prettyResponse (Unit mu) = "Unit " ++ show mu
prettyResponse (ByteString (Just bs)) =
  "ByteString \"" ++ prettyRunLenEnc (runLengthEncoding bs) ++ "\""
prettyResponse (ByteString Nothing) =
  "ByteString Nothing"
prettyResponse (IOException e) = "IOException " ++ displayException e

type Model = FakeJournal

-- If there's nothing new to read, then don't generate reads (because they are
-- blocking) and don't append empty bytestrings.
precondition :: Model -> Command -> Bool
precondition m ReadJournal   = Vector.length (fjJournal m) /= fjIndex m
precondition m (AppendBS bs) =
  not (BS.null bs) && BS.length bs + hEADER_LENGTH <= oTermBufferLength testOptions

step :: Command -> Model -> (Model, Response)
step (AppendBS bs) m = Unit <$> appendBSFake bs m
step ReadJournal   m = ByteString <$> readJournalFake m

exec :: Command -> Journal -> IO Response
exec (AppendBS bs) j = Unit <$> appendBS j bs
exec ReadJournal   j = ByteString <$> readJournal j

-- Generates ASCII bytestrings.
genByteString :: Gen ByteString
genByteString = oneof (map genBs [65..90]) -- A-Z
  where
    genBs :: Int -> Gen ByteString
    genBs i = frequency
      [ (1, sized (\n -> return (BS.replicate n (fromIntegral i))))
      , (1, return (BS.replicate  maxLen      (fromIntegral i)))
      , (1, return (BS.replicate (maxLen - 1) (fromIntegral i)))
      ]
    maxLen = oTermBufferLength testOptions - hEADER_LENGTH - fOOTER_LENGTH

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

testOptions :: Options
testOptions = defaultOptions

prop_journal :: Property
prop_journal =
  let m = startJournalFake in
  forAllShrink (genCommands m) (shrinkCommands m) $ \cmds -> monadicIO $ do
    -- run (putStrLn ("Generated commands: " ++ show cmds))
    tmp <- run (canonicalizePath =<< getTemporaryDirectory)
    (fp, h) <- run (openTempFile tmp "JournalTest")
    run (allocateJournal fp testOptions)
    j <- run (startJournal fp testOptions)
    monitor (tabulate "Commands" (map constructorString cmds))
    (result, hist) <- go cmds m j []
    -- run (uncurry stopJournal j)
    monitorStats (stats (zip cmds hist))
    run (removeFile fp)
    return result
    where
      go []          _m _j hist = return (True, reverse hist)
      go (cmd : cmds) m  j hist = do
        let (m', resp) = step cmd m
        resp' <- run (exec cmd j `catch` (return . IOException))
        assertWithFail (resp == resp') $
          prettyResponse resp ++ " /= " ++ prettyResponse resp'
        go cmds m' j (resp : hist)

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
  , sRotations    = totalAppended `div` oTermBufferLength testOptions
  }
  where
    Sum totalAppended =
      foldMap (\(cmd, _resp) ->
                 case cmd of
                   AppendBS bs -> Sum (hEADER_LENGTH + BS.length bs)
                   _otherwise  -> mempty) hist

monitorStats :: Monad m => Stats -> PropertyM m ()
monitorStats stats
  = monitor
  $ collect ("Bytes written: " <> show (sBytesWritten stats))
  . collect ("Rotations: "     <> show (sRotations stats))

runCommands :: [Command] -> IO Bool
runCommands cmds = do
  let m = startJournalFake
  withTempFile "runCommands" $ \fp _handle -> do
    allocateJournal fp testOptions
    j <- startJournal fp testOptions
    putStrLn ""
    go m j cmds []
  where
    go :: Model -> Journal -> [Command] -> [(Command, Response)] -> IO Bool
    go m j [] _hist = putStrLn "\nSuccess!" >> return True
    go m j (cmd : cmds) hist = do
      let (m', resp) = step cmd m
      putStrLn (prettyFakeJournal m)
      putStrLn ""
      putStrLn ("    == " ++ prettyCommand cmd ++ " ==> " ++ prettyResponse resp)
      putStrLn ""
      if null cmds
      then putStrLn (prettyFakeJournal m')
      else return ()
      resp' <- exec cmd j `catch` (return . IOException)
      -- is <- checkForInconsistencies (fst j)
      if resp == resp' -- && null is
      then go m' j cmds ((cmd, resp) : hist)
      else do
        putStrLn ""
        when (resp /= resp') $
          putStrLn ("Failed: " ++ prettyResponse resp ++ " /= " ++ prettyResponse resp')
        -- when (not (null is)) $
        --   putStrLn ("Inconsistencies: " ++ inconsistenciesString is)
        putStrLn ""
        putStrLn "Journal dump:"
        dumpJournal j
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
