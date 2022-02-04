{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE OverloadedStrings #-}

module JournalTest where

import Debug.Trace (trace)
import Control.Arrow ((&&&))
import Control.Exception (IOException, catch, displayException)
import Control.Monad (unless, when)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Builder as BS
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy as LBS
import Data.Monoid (Sum(Sum))
import Data.Vector (Vector)
import qualified Data.Vector as Vector
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
import Journal.Internal.Utils hiding (assert)

------------------------------------------------------------------------

data FakeJournal' a = FakeJournal
  { fjJournal   :: Vector a
  , fjIndex     :: Int
  , fjTermCount :: Int
  }
  deriving (Show, Functor)

type FakeJournal = FakeJournal' ByteString

prettyFakeJournal :: FakeJournal -> String
prettyFakeJournal = show . fmap (prettyRunLenEnc . encodeRunLength)

startJournalFake :: FakeJournal
startJournalFake = FakeJournal Vector.empty 0 1

appendBSFake :: ByteString -> FakeJournal -> (FakeJournal, Either AppendError ())
appendBSFake bs fj@(FakeJournal bss ix termCount) =
  trace (unlines [ "TRACE"
                 , "ix: " ++ show ix
                 , "termCount: " ++ show termCount
                 , "bs: " ++ show (encodeRunLength bs)
                 , "readBytes: " ++ show readBytes
                 , "unreadBytes: " ++  show unreadBytes
                 , "position: " ++ show position
                 , "limit: " ++ show limit
                 ]) $
    if position < limit
    then if journalLength >= termLen * termCount
         then (FakeJournal (Vector.snoc bss padding) ix (termCount + 1), Left Rotation)
         else (FakeJournal (Vector.snoc bss bs) ix termCount, Right ())
    else (fj, Left BackPressure)
  where
    journalLength :: Int
    journalLength = sum (Vector.map (\bs -> hEADER_LENGTH + BS.length bs) bss)
                  + hEADER_LENGTH + BS.length bs

    padding :: ByteString
    padding = BS.replicate (termLen * termCount - journalLength) '0'

    termLen :: Int
    termLen = oTermBufferLength testOptions

    position = readBytes + unreadBytes

    limit :: Int
    limit = readBytes + termLen `div` 2

    readBytes :: Int
    readBytes = sum [ hEADER_LENGTH + BS.length bs
                    | bs <- map (bss Vector.!) [0..ix - 1]
                    ]

    unreadBytes :: Int
    unreadBytes = sum [ hEADER_LENGTH + BS.length bs
                      | bs <- map (bss Vector.!) [ix..Vector.length bss - 1]
                      ]

readJournalFake :: FakeJournal -> (FakeJournal, Maybe ByteString)
readJournalFake fj@(FakeJournal jour ix termCount)
  -- Nothing to read:
  | Vector.length jour == ix = (fj, Nothing)
  -- Padding, skip:
  | BS.length (jour Vector.! ix) == 0 || BS.head (jour Vector.! ix) == '0' =
      readJournalFake (fj { fjIndex = ix + 1 })
  -- Normal read:
  | otherwise =  (FakeJournal jour (ix + 1) termCount, Just (jour Vector.! ix))

------------------------------------------------------------------------

data Command
  = AppendBS [(Int, Char)] -- Run length encoded bytestring.
  -- Tee
  -- AppendRecv
  | ReadJournal
  -- SaveSnapshot
  -- TruncateAfterSnapshot
  -- LoadSnapshot
  -- Replay
  | DumpJournal
  deriving Show

constructorString :: Command -> String
constructorString AppendBS {} = "AppendBS"
constructorString ReadJournal = "ReadJournal"
constructorString DumpJournal = "DumpJournal"

prettyCommand :: Command -> String
prettyCommand = show

prettyCommands :: [Command] -> String
prettyCommands = concat . go ["["] . map prettyCommand
  where
    go :: [String] -> [String] -> [String]
    go acc []       = reverse ("]" : acc)
    go acc [s]      = reverse ("]" : s : acc)
    go acc (s : ss) = go (", " : s : acc) ss

encodeRunLength :: ByteString -> [(Int, Char)]
encodeRunLength = map (BS.length &&& BS.head) . BS.group

decodeRunLength :: [(Int, Char)] -> ByteString
decodeRunLength = go mempty
  where
    go :: BS.Builder -> [(Int, Char)] -> ByteString
    go acc []             = LBS.toStrict (BS.toLazyByteString acc)
    go acc ((n, c) : ncs) = go (acc <> BS.byteString (BS.replicate n c)) ncs

prop_runLengthEncoding :: ByteString -> Property
prop_runLengthEncoding bs = bs === decodeRunLength (encodeRunLength bs)

prop_runLengthEncoding' :: Property
prop_runLengthEncoding' = forAll genRunLenEncoding $ \rle ->
  rle === encodeRunLength (decodeRunLength rle)

prettyRunLenEnc :: [(Int, Char)] -> String
prettyRunLenEnc ncs0 = case ncs0 of
  []           -> ""
  [(n, c)]     -> go n c
  (n, c) : ncs -> go n c ++ " " ++ prettyRunLenEnc ncs
  where
    go 1 c = [ c ]
    go n c = show n ++ "x" ++ [ c ]

data Response
  = Result (Either AppendError ())
  | ByteString (Maybe ByteString)
  | IOException IOException
  deriving Eq

prettyResponse :: Response -> String
prettyResponse (Result eu) = "Result (" ++ show eu ++ ")"
prettyResponse (ByteString (Just bs)) =
  "ByteString \"" ++ prettyRunLenEnc (encodeRunLength bs) ++ "\""
prettyResponse (ByteString Nothing) =
  "ByteString Nothing"
prettyResponse (IOException e) = "IOException " ++ displayException e

type Model = FakeJournal

-- If there's nothing new to read, then don't generate reads (because they are
-- blocking) and don't append empty bytestrings.
precondition :: Model -> Command -> Bool
precondition m ReadJournal    = Vector.length (fjJournal m) /= fjIndex m
precondition m (AppendBS rle) = let bs = decodeRunLength rle in
  not (BS.null bs) && BS.length bs + hEADER_LENGTH < oTermBufferLength testOptions `div` 2
precondition m DumpJournal = True

step :: Command -> Model -> (Model, Response)
step (AppendBS rle) m = Result <$> appendBSFake (decodeRunLength rle) m
step ReadJournal    m = ByteString <$> readJournalFake m
step DumpJournal    m = (m, Result (Right ()))

exec :: Command -> Journal -> IO Response
exec (AppendBS rle) j = Result <$> appendBS j (decodeRunLength rle)
exec ReadJournal    j = ByteString <$> readJournal j
exec DumpJournal    j = Result . Right <$> dumpJournal j

genRunLenEncoding :: Gen [(Int, Char)]
genRunLenEncoding = sized $ \n -> do
  len <- elements [ max 1 n -- Disallow n == 0.
                  , maxLen, maxLen - 1]
  chr <- elements ['A'..'Z']
  return [(len, chr)]
  where
    maxLen = (oTermBufferLength testOptions - hEADER_LENGTH - fOOTER_LENGTH) `div` 2

genCommand :: Gen Command
genCommand = frequency
  [ (1, AppendBS <$> genRunLenEncoding)
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
shrinkCommand ReadJournal    = []
shrinkCommand (AppendBS rle) =
  [ AppendBS rle'
  | rle' <- shrinkList (\(i, c) -> [ (i', c) | i' <- shrink i ]) rle
  , not (null rle')
  ]

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
  forAllShrinkShow (genCommands m) (shrinkCommands m) prettyCommands $ \cmds -> monadicIO $ do
    -- run (putStrLn ("Generated commands: " ++ show cmds))
    tmp <- run (canonicalizePath =<< getTemporaryDirectory)
    (fp, h) <- run (openTempFile tmp "JournalTest")
    run (allocateJournal fp testOptions)
    j <- run (startJournal fp testOptions)
    monitor (tabulate "Commands" (map constructorString cmds))
    monitor (whenFail (dumpJournal j))
    (result, hist) <- go cmds m j []
    -- run (uncurry stopJournal j)
    -- monitorStats (stats (zip cmds hist))
    run (removeFile fp)
    return result
    where
      go :: [Command] -> Model -> Journal -> [Response] -> PropertyM IO (Bool, [Response])
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
  {-
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
-}

runCommands :: [Command] -> IO Bool
runCommands cmds = do
  let m = startJournalFake
  withTempFile "runCommands" $ \fp _handle -> do
    allocateJournal fp testOptions
    j <- startJournal fp testOptions
    putStrLn ""
    b <- go m j cmds []
    dumpJournal j
    return b
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
        -- print (stats (reverse hist))
        return False

------------------------------------------------------------------------

-- XXX: make sure all these unit tests are part of the coverage...

unit_bug0 :: Assertion
unit_bug0 = assertProgram ""
  [ AppendBS [(2, 'E')]
  , AppendBS [(32752, 'O')]
  ]

unit_bug1 :: Assertion
unit_bug1 = assertProgram ""
  [ AppendBS [(32756, 'O')]
  , AppendBS [(32756, 'G')]
  ]

unit_bug2 :: Assertion
unit_bug2 = assertProgram ""
  [ AppendBS [(32756, 'O')]
  , ReadJournal
  , AppendBS [(32756, 'G')]
  , ReadJournal
  , AppendBS [(32756, 'K')]
  , DumpJournal
  , ReadJournal
  , AppendBS [(32756, 'J')]
  ]

unit_bug3 :: Assertion
unit_bug3 = assertProgram ""
  [ AppendBS [(7,'N')]
  , ReadJournal
  , AppendBS [(32756,'N')]
  , ReadJournal
  , AppendBS [(32756,'W')] -- 7+6 + 32756+6 + 32756+6 = 65537 which won't fit in
                           -- the first term, so padding is written, term
                           -- rotation happens, and the Ws get written to the
                           -- next term. The padding will be pretty big:
                           -- 65536-(7+6 + 32756+6) = 32761, so after the Ws are
                           -- written there's 32761 + 32756+6 = 65523 unread
                           -- bytes. 7+6+32756+6 = 32775 bytes have been read,
                           -- so the limit is 32775 + (64*1024 / 2) = 65543.
  , AppendBS [(32756,'Q')]
  ]

unit_bug4 :: Assertion
unit_bug4 = assertProgram ""
  [AppendBS [(1, 'A')], AppendBS [(32755,'Q')], AppendBS [(1,'D')]]

unit_bug5 :: Assertion
unit_bug5 = assertProgram ""
  [AppendBS [(1,'K')], AppendBS [(32757,'Q')], ReadJournal, AppendBS [(32761,'R')]]
-- ^ Before writing the Rs, we have posiiton: 1+6+32757+6 = 32770 and limit:
-- 1+6+(64*1024/2) = 32775, so that's fine, however 32770+32761+6 = 65537 so it
-- doesn't fit in the current term. 65536-(1+6+32757+6) = 32766 bytes of padding
-- is written instead and then term rotation happens before the 32761+6 bytes
-- related to the append of Rs, this gives us a position of 32770+32766 = 65536
-- which is over the limit and so backpressure should happen.

unit_bug6 :: Assertion
unit_bug6 = assertProgram ""
  [ AppendBS [(1,'J')], ReadJournal, AppendBS [(32757,'K')] -- 1+6+32757+6 = 32770

  , AppendBS [(32761,'H')] -- 32770+32761+6 = 65537 doesn't fit in term,
                           -- 65536-32770 = 32766 padding is written instead,
                           -- the Hs get discarded, resulting in a position of
                           -- 32770+32766=65536, while the limit is
                           -- 1+6+(64*1024/2)=32775, so backpressure should
                           -- happen (which it does in both the model and SUT).

  , AppendBS [(1,'F')]]    -- Here backpressure should happen again, but it
                           -- doesn't in the model, because Hs has been
                           -- discarded previously it's not part of the padding
                           -- calculation...


unit_bug7 :: Assertion
unit_bug7 = assertProgram ""
  [AppendBS [(32756,'Y')], AppendBS [(32761,'A')], ReadJournal, AppendBS [(1,'J')]]


------------------------------------------------------------------------

assertProgram :: String -> [Command] -> Assertion
assertProgram msg cmds = do
  b <- runCommands cmds
  assertBool msg b
