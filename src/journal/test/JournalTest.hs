{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE OverloadedStrings #-}

module JournalTest where

import Control.Arrow ((&&&))
import Control.Concurrent (ThreadId, myThreadId, threadDelay)
import Control.Concurrent.Async (mapConcurrently)
import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TQueue
       (TQueue, flushTQueue, newTQueueIO, writeTQueue)
import Control.Exception (IOException, catch, displayException)
import Control.Monad (replicateM_, unless, when)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Builder as BS
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy as LBS
import Data.Int (Int64)
import Data.List (permutations)
import Data.Monoid (Sum(Sum))
import Data.Time (diffUTCTime, getCurrentTime)
import Data.Tree (Forest, Tree(Node))
import Data.Vector (Vector)
import qualified Data.Vector as Vector
import Debug.Trace (trace)
import System.Directory
       (canonicalizePath, getTemporaryDirectory, removeFile)
import System.IO (openTempFile)
import System.Random (randomRIO)
import System.Timeout (timeout)
import Test.QuickCheck
import Test.QuickCheck.Instances.ByteString ()
import Test.QuickCheck.Monadic
import Test.Tasty.HUnit (Assertion, assertBool)

import Journal
import Journal.Internal
import Journal.Internal.Logger (ioLogger, nullLogger)
import Journal.Internal.Utils hiding (assert)
import qualified Journal.MP as MP

------------------------------------------------------------------------

data FakeJournal' a = FakeJournal
  { fjJournal   :: Vector a
  , fjIndex     :: Int
  , fjTermCount :: Int
  }
  deriving (Eq, Show, Functor)

type FakeJournal = FakeJournal' ByteString

prettyFakeJournal :: FakeJournal -> String
prettyFakeJournal = show . fmap (prettyRunLenEnc . encodeRunLength)

initModel :: FakeJournal
initModel = FakeJournal Vector.empty 0 1

eNABLE_TRACING :: Bool
eNABLE_TRACING = False

doTrace :: String -> a -> a
doTrace | eNABLE_TRACING = trace
        | otherwise      = const id

appendBSFake :: ByteString -> FakeJournal -> [(FakeJournal, Either AppendError ())]
appendBSFake bs fj@(FakeJournal bss ix termCount) =
  doTrace
    (unlines [ "TRACE"
             , "ix: " ++ show ix
             , "termCount: " ++ show termCount
             , "bs: " ++ show (encodeRunLength bs)
             , "readBytes: " ++ show readBytes
             , "unreadBytes: " ++  show unreadBytes
             , "position: " ++ show position
             , "limit: " ++ show limit
             , "journalLength': " ++ show journalLength'
             , "termLen * termCount: " ++ show (termLen * termCount)
             ]) $
    if position < limit
    then [noBackPressure]
    else if position < hardLimit
         then [backPressure, noBackPressure]
         else [backPressure]
  where
    backPressure = (fj, Left BackPressure)
    noBackPressure =
      if journalLength' > termLen * termCount
      then (FakeJournal (if BS.length padding == 0
                           then bss
                           else Vector.snoc bss padding) ix (termCount + 1), Left Rotation)
      else (FakeJournal (Vector.snoc bss bs) ix termCount, Right ())

    journalLength :: Int
    journalLength = sum (Vector.map
                         (\bs -> align (hEADER_LENGTH + BS.length bs) fRAME_ALIGNMENT) bss)
    journalLength' :: Int
    journalLength' = journalLength + align (hEADER_LENGTH + BS.length bs) fRAME_ALIGNMENT

    padding :: ByteString
    padding = BS.replicate (termLen * termCount - journalLength - hEADER_LENGTH) '0'

    termLen :: Int
    termLen = oTermBufferLength testOptions

    position = readBytes + unreadBytes

    limit :: Int
    limit = readBytes + termLen `div` 2

    hardLimit :: Int
    hardLimit = readBytes + termLen -- TODO: calculate this correctly

    readBytes :: Int
    readBytes = Vector.sum
              . Vector.map (\bs -> align (hEADER_LENGTH + BS.length bs) fRAME_ALIGNMENT)
              . Vector.slice 0 (max 0 (ix - 1))
              $ bss

    unreadBytes :: Int
    unreadBytes = Vector.sum
                . Vector.map (\bs -> align (hEADER_LENGTH + BS.length bs) fRAME_ALIGNMENT)
                . Vector.slice ix (max 0 (Vector.length bss - 1 - ix))
                $ bss

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
  deriving (Eq, Show)

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
  not (BS.null bs) &&
  align (BS.length bs + hEADER_LENGTH) fRAME_ALIGNMENT <= oTermBufferLength testOptions `div` 2
precondition m DumpJournal = True

step' :: Command -> Model -> [(Model, Response)]
step' (AppendBS rle) m = fmap Result <$> appendBSFake (decodeRunLength rle) m
step' ReadJournal    m = pure $ ByteString <$> readJournalFake m
step' DumpJournal    m = pure $ (m, Result (Right ()))

-- step is like step' but gives only one answer, if step' gave multiple, then we pick the BackPressure one
step :: Command -> Model -> (Model, Response)
step cmd m = case step' cmd m of
  [] -> error "Model didn't predict anything"
  [r] -> r
  xs -> let res = (m, Result (Left BackPressure)) in
    if res `elem` xs
    then res
    else error $ "Model gave multiple results."

exec :: Command -> Journal -> IO Response
exec (AppendBS rle) j = Result <$> appendBS j (decodeRunLength rle)
exec ReadJournal    j = ByteString <$> readJournal j
exec DumpJournal    j = Result . Right <$> dumpJournal j

genRunLenEncoding :: Gen [(Int, Char)]
genRunLenEncoding = sized $ \n -> do
  len <- elements [ max 1 n -- Disallow n == 0.
                  , maxLen
                  , maxLen - 1
                  ]
  chr <- elements ['A'..'Z']
  return [(len, chr)]
  where
    maxLen = oTermBufferLength testOptions `div` 2 - hEADER_LENGTH

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
testOptions = defaultOptions { oLogger = nullLogger }

forAllCommands :: ([Command] -> Property) -> Property
forAllCommands k =
  forAllShrinkShow (genCommands m) (shrinkCommands m) prettyCommands k
  where
    m :: Model
    m = initModel

timeIt :: IO a -> IO (a, Double)
timeIt io = do
  start <- getCurrentTime
  x <- io
  end <- getCurrentTime
  return (x, realToFrac (diffUTCTime end start * 1000 * 1000))

initJournal :: IO (FilePath, Journal)
initJournal = do
  tmp <- canonicalizePath =<< getTemporaryDirectory
  (fp, h) <- openTempFile tmp "JournalTest"
  allocateJournal fp testOptions
  jour <- startJournal fp testOptions
  return (fp, jour)

prop_journal :: Property
prop_journal =
  let m = initModel in
  forAllCommands $ \cmds -> monadicIO $ do
    -- run (putStrLn ("Generated commands: " ++ show cmds))
    (fp, j) <- run initJournal
    monitor (tabulate "Commands" (map constructorString cmds))
    monitor (classifyCommandsLength cmds)
    monitor (whenFail (dumpJournal j))
    (result, hist) <- go cmds m j []
    written <- run (metricsBytesWritten j)
    monitor (classifyBytesWritten written)
    monitor (classifyLatencies (zip cmds (map snd hist)))
    run (removeFile fp)
    return result
    where
      go :: [Command] -> Model -> Journal -> [(Response, Double)]
         -> PropertyM IO (Bool, [(Response, Double)])
      go []          _m _j hist = return (True, reverse hist)
      go (cmd : cmds) m  j hist = do
        let (m', resp) = step cmd m
        (resp', t) <- run (timeIt (exec cmd j `catch` (return . IOException)))
        assertWithFail (resp == resp') $
          prettyResponse resp ++ " /= " ++ prettyResponse resp'
        go cmds m' j ((resp, t) : hist)

assertWithFail :: Monad m => Bool -> String -> PropertyM m ()
assertWithFail condition msg = do
  unless condition $
    monitor (counterexample ("Failed: " ++ msg))
  assert condition

classifyLatencies :: [(Command, Double)] -> Property -> Property
classifyLatencies []             = id
classifyLatencies ((c, t) : cts)
  = classify (0  < t && t <= 5)  ("latency " ++ constructorString c ++ ": 0-5ns")
  . classify (5  < t && t <= 10) ("latency " ++ constructorString c ++ ": 5-10ns")
  . classify (10 < t && t <= 20) ("latency " ++ constructorString c ++ ": 11-20ns")
  . classify (20 < t && t <= 30) ("latency " ++ constructorString c ++ ": 21-30ns")
  . classify (t > 30)            ("latency " ++ constructorString c ++ ": >30ns")
  . classifyLatencies cts

classifyLatencies' :: [(Command, Double)] -> Property -> Property
classifyLatencies' cts =
  classifyTable cts snd (buckets 0 30 5) (\(cmd, _) (lo, hi) ->
    "latency " ++ constructorString cmd ++ ": " ++ show lo ++ "-" ++ show hi ++ "ns")

buckets :: Num n => n -> n -> n -> [(n, n)]
buckets lo hi step = undefined

classifyTable :: Num n => [a] -> (a -> n) -> [(n, n)] -> (a -> (n, n) -> String)
              -> Property -> Property
classifyTable [] _ _ _ = id
classifyTable (x : xs) f buckets g
  = undefined

classifyCommandsLength :: [Command] -> Property -> Property
classifyCommandsLength cmds
  = classify (length cmds == 0)                        "length commands: 0"
  . classify (0   < length cmds && length cmds <= 10)  "length commands: 1-10"
  . classify (10  < length cmds && length cmds <= 50)  "length commands: 11-50"
  . classify (50  < length cmds && length cmds <= 100) "length commands: 51-100"
  . classify (100 < length cmds && length cmds <= 200) "length commands: 101-200"
  . classify (200 < length cmds && length cmds <= 500) "length commands: 201-500"
  . classify (500 < length cmds)                       "length commands: >501"

classifyBytesWritten :: Int64 -> Property -> Property
classifyBytesWritten bytes
  = classify (bytes == 0)
             "bytes written: 0"
  . classify (0 < bytes && bytes <= termBufferLen)                     (msg 0 1)
  . classify (1 * termBufferLen < bytes && bytes <= 2  * termBufferLen) (msg 1 2)
  . classify (2 * termBufferLen < bytes && bytes <= 3  * termBufferLen) (msg 2 3)
  . classify (3 * termBufferLen < bytes && bytes <= 4  * termBufferLen) (msg 3 4)
  . classify (4 * termBufferLen < bytes && bytes <= 5  * termBufferLen) (msg 4 5)
  . classify (5 * termBufferLen < bytes && bytes <= 6  * termBufferLen) (msg 5 6)
  . classify (6 * termBufferLen < bytes && bytes <= 7  * termBufferLen) (msg 6 7)
  . classify (7 * termBufferLen < bytes && bytes <= 8  * termBufferLen) (msg 7 8)
  . classify (8 * termBufferLen < bytes && bytes <= 9  * termBufferLen) (msg 8 9)
  . classify (9 * termBufferLen < bytes && bytes <= 10 * termBufferLen) (msg 9 10)
  . classify (10 * termBufferLen < bytes)
             ("bytes written: >" ++ show (10 * termBufferLen))
  where
    msg low high = concat
      ["bytes written: ", show (low * termBufferLen), "-", show (high * termBufferLen)]
    termBufferLen = int2Int64 (oTermBufferLength testOptions)

runCommands :: [Command] -> IO Bool
runCommands cmds = do
  let m = initModel
  withTempFile "runCommands" $ \fp _handle -> do
    allocateJournal fp testOptions
    j <- startJournal fp testOptions
    putStrLn ""
    b <- go m j cmds
    dumpJournal j
    return b
  where
    go :: Model -> Journal -> [Command] -> IO Bool
    go m j [] = putStrLn "\nSuccess!" >> return True
    go m j (cmd : cmds) = do
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
      then go m' j cmds
      else do
        putStrLn ""
        when (resp /= resp') $
          putStrLn ("Failed: " ++ prettyResponse resp ++ " /= " ++ prettyResponse resp')
        -- when (not (null is)) $
        --   putStrLn ("Inconsistencies: " ++ inconsistenciesString is)
        putStrLn ""
        putStrLn "Journal dump:"
        dumpJournal j
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
  [AppendBS [(1,'K')], AppendBS [(32757,'Q')], ReadJournal, AppendBS [(32759,'R')]]
-- ^ Before writing the Rs, we have posiiton: 1+6+32757+6 = 32770 and limit:
-- 1+6+(64*1024/2) = 32775, so that's fine, however 32770+32761+6 = 65537 so it
-- doesn't fit in the current term. 65536-(1+6+32757+6) = 32766 bytes of padding
-- is written instead and then term rotation happens before the 32761+6 bytes
-- related to the append of Rs, this gives us a position of 32770+32766 = 65536
-- which is over the limit and so backpressure should happen.

unit_bug6 :: Assertion
unit_bug6 = assertProgram ""
  [ AppendBS [(1,'J')], ReadJournal, AppendBS [(32757,'K')] -- 1+8+32757+8 = 32774

  , AppendBS [(32755,'H')] -- 32774+32755+8 = 65537 doesn't fit in term,
                           -- 65536-32774 = 32762 padding is written instead,
                           -- the Hs get discarded, resulting in a position of
                           -- 32774+32762=65536, while the limit is
                           -- 1+6+(64*1024/2)=32775, so backpressure should
                           -- happen (which it does in both the model and SUT).

  , AppendBS [(1,'F')]]    -- Here backpressure should happen again, but it
                           -- doesn't in the model, because Hs has been
                           -- discarded previously it's not part of the padding
                           -- calculation...

unit_bug7 :: Assertion
unit_bug7 = assertProgram ""
  [AppendBS [(32756,'Y')], AppendBS [(32756,'A')], ReadJournal, AppendBS [(1,'J')]]

unit_bug8 :: Assertion
unit_bug8 = assertProgram ""
  [AppendBS [(32756,'I')], ReadJournal, AppendBS [(16381,'Q')], AppendBS [(16381,'U')]]

unit_bug9 :: Assertion
unit_bug9 = assertProgram ""
  [AppendBS [(32754,'L')], ReadJournal, AppendBS [(32759,'K')], AppendBS [(1,'M')]]

unit_bug10 :: Assertion
unit_bug10 = assertProgram ""
  [ AppendBS [(1,'K')], AppendBS [(1,'P')], ReadJournal, ReadJournal
  , AppendBS [(1,'X')], ReadJournal
  ]

unit_bug11 :: Assertion
unit_bug11 = assertProgram ""
  [ AppendBS [(32753,'A')], ReadJournal
  , AppendBS [(32753,'B')], ReadJournal
  , AppendBS [(32753,'C')], ReadJournal
  , AppendBS [(32753,'D')], ReadJournal
  , AppendBS [(32753,'E')], ReadJournal
  , AppendBS [(32753,'F')], ReadJournal
  , AppendBS [(32753,'G')], ReadJournal
  , AppendBS [(32753,'H')], ReadJournal
  ]

unit_bug12 :: Assertion
unit_bug12 = assertProgram ""
  [AppendBS [(1,'E')], AppendBS [(32745,'A')], ReadJournal, AppendBS [(32753,'U')]]

unit_bug13 :: Assertion
unit_bug13 = assertProgram ""
  [ AppendBS [(32737,'Q')], ReadJournal, AppendBS [(9,'I')]
  , AppendBS [(32753,'W')], AppendBS [(1,'U')]
  ]

unit_bug14 :: Assertion
unit_bug14 = assertProgram ""
  [ AppendBS [(32737,'H')], ReadJournal, AppendBS [(9,'D')]
  , AppendBS [(32753,'F')], ReadJournal, AppendBS [(1,'Z')]]

unit_bug15 :: Assertion
unit_bug15 = assertConcProgram "" $ ConcProgram
  [[AppendBS [(1024,'Z')],AppendBS [(1024,'U')],AppendBS [(1024,'C')],AppendBS [(1024,'Q')]],[AppendBS [(1024,'B')],AppendBS [(1024,'E')],AppendBS [(1024,'P')],ReadJournal,ReadJournal],[ReadJournal,ReadJournal,ReadJournal,ReadJournal],[AppendBS [(1024,'P')],AppendBS [(1024,'I')],ReadJournal],[ReadJournal,AppendBS [(1024,'S')],ReadJournal,AppendBS [(1024,'X')]],[AppendBS [(1024,'V')],AppendBS [(1024,'N')],ReadJournal,AppendBS [(1024,'C')],AppendBS [(1024,'V')]],[AppendBS [(1024,'R')],ReadJournal],[ReadJournal,AppendBS [(1024,'V')]],[ReadJournal,ReadJournal,AppendBS [(1024,'E')]],[ReadJournal,ReadJournal,ReadJournal,ReadJournal],[AppendBS [(1024,'W')],AppendBS [(1024,'O')],AppendBS [(1024,'P')]],[AppendBS [(1024,'B')],AppendBS [(1024,'H')],ReadJournal,ReadJournal,ReadJournal], [ReadJournal,ReadJournal],[AppendBS [(1024,'E')],AppendBS [(1024,'B')],AppendBS [(1024,'I')],AppendBS [(1024,'F')],AppendBS [(1024,'P')]],[AppendBS [(1024,'Q')],ReadJournal,AppendBS [(1024,'C')],ReadJournal,ReadJournal],[ReadJournal,AppendBS [(1024,'H')],ReadJournal,AppendBS [(1024,'P')],AppendBS [(1024,'L')]],[AppendBS [(1024,'X')],ReadJournal,AppendBS [(1024,'Y')],ReadJournal,ReadJournal],[AppendBS [(1024,'H')],ReadJournal],[ReadJournal,ReadJournal,ReadJournal,ReadJournal,AppendBS [(1024,'Q')]],[AppendBS [(1024,'J')],AppendBS [(1024,'N')],AppendBS [(1024,'D')],ReadJournal,AppendBS [(1024,'S')]],[ReadJournal,ReadJournal,AppendBS [(1024,'S')]],[AppendBS [(1024,'Z')],ReadJournal,AppendBS [(1024,'W')],AppendBS [(1024,'E')]],[ReadJournal,ReadJournal,ReadJournal],[ReadJournal,ReadJournal,AppendBS [(1024,'I')],AppendBS [(1024,'W')],AppendBS [(1024,'M')]],[AppendBS [(1024,'F')],AppendBS [(1024,'L')]],[ReadJournal,AppendBS [(1024,'J')],ReadJournal,ReadJournal],[AppendBS [(1024,'R')],ReadJournal],[AppendBS [(1024 ,'A')],ReadJournal,AppendBS [(1024,'N')],AppendBS [(1024,'W')]],[ReadJournal,AppendBS [(1024,'N')],ReadJournal],[ReadJournal,AppendBS [(1024,'C')]],[AppendBS [(1024,'I')],ReadJournal,AppendBS [(1024,'F')] ,AppendBS [(1024,'O')]],[AppendBS [(1024,'A')],ReadJournal,ReadJournal],[AppendBS [(1024,'W')],AppendBS [(1024,'Y')],AppendBS [(1024,'P')]],[ReadJournal,ReadJournal],[AppendBS [(1024,'S')],ReadJournal],[AppendBS [(1024,'L')],ReadJournal],[AppendBS [(1024,'D')],ReadJournal,ReadJournal,ReadJournal,ReadJournal],[ReadJournal,AppendBS [(1024,'P')],AppendBS [(1024,'E')],AppendBS [(1024,'K')]]]

alignedLength :: Int -> Int
alignedLength n = align (hEADER_LENGTH + n) fRAME_ALIGNMENT

prop_alignment :: Positive Int -> Bool
prop_alignment (Positive i) = align i fRAME_ALIGNMENT `mod` fRAME_ALIGNMENT == 0

------------------------------------------------------------------------

assertProgram :: String -> [Command] -> Assertion
assertProgram msg cmds = do
  b <- runCommands cmds
  assertBool msg b

assertConcProgram :: String -> ConcProgram -> Assertion
assertConcProgram msg (ConcProgram cmdss) = do
  (fp, jour) <- initJournal
  queue <- newTQueueIO
  mapM_ (mapConcurrently (concExec queue jour)) cmdss
  hist <- History <$> atomically (flushTQueue queue)
  removeFile fp
  let msg' = msg ++ "\nHistory:\n" ++ prettyHistory hist
  assertBool msg' (linearisable (interleavings hist))

prettyHistory :: History -> String
prettyHistory = show

------------------------------------------------------------------------

newtype ConcProgram = ConcProgram { unConcProgram :: [[Command]] }
  deriving Show

forAllConcProgram :: (ConcProgram -> Property) -> Property
forAllConcProgram k =
  forAllShrinkShow (genConcProgram m) (shrinkConcProgram m) prettyConcProgram k
  where
    m = initModel

genConcProgram :: Model -> Gen ConcProgram
genConcProgram m0 = sized (go m0 [])
  where
    go :: Model -> [[Command]] -> Int -> Gen ConcProgram
    go m acc sz | sz <= 0   = return (ConcProgram (reverse acc))
                | otherwise = do
                    n <- chooseInt (2, 5)
                    cmds <- vectorOf n genCommand `suchThat` concSafe m
                    go (advanceModel m cmds) (cmds : acc) (sz - n)

advanceModel :: Model -> [Command] -> Model
advanceModel m cmds = foldl (\ih cmd -> fst (step cmd ih)) m cmds

concSafe :: Model -> [Command] -> Bool
concSafe m0 = all (validProgram m0) . permutations

validConcProgram :: Model -> ConcProgram -> Bool
validConcProgram m0 (ConcProgram cmdss0) = go m0 True cmdss0
  where
    go :: Model -> Bool -> [[Command]] -> Bool
    go m False _              = False
    go m acc   []             = acc
    go m acc   (cmds : cmdss) = go (advanceModel m cmds) (concSafe m cmds) cmdss

shrinkConcProgram :: Model -> ConcProgram -> [ConcProgram]
shrinkConcProgram m
  = filter (validConcProgram m)
  . map ConcProgram
  . shrinkList (shrinkCommands m)
  . unConcProgram

prettyConcProgram :: ConcProgram -> String
prettyConcProgram = show

newtype History = History [Operation]
  deriving Show

newtype Pid = Pid Int
  deriving (Eq, Ord, Show)

data Operation
  = Invoke Pid Command
  | Ok     Pid Response
  deriving Show

toPid :: ThreadId -> Pid
toPid tid = Pid (read (drop (length ("ThreadId " :: String)) (show tid)))

concExec :: TQueue Operation -> Journal -> Command -> IO ()
concExec queue jour cmd = do
  pid <- toPid <$> myThreadId
  atomically (writeTQueue queue (Invoke pid cmd))
  -- Adds some entropy to the possible interleavings.
  sleep <- randomRIO (5, 20)
  threadDelay sleep
  resp <- execMP cmd jour
  atomically (writeTQueue queue (Ok pid resp))

execMP :: Command -> Journal -> IO Response
execMP (AppendBS rle) j = Result <$> MP.appendBS j (decodeRunLength rle)
execMP ReadJournal    j = ByteString <$> MP.readJournal j
execMP DumpJournal    j = Result . Right <$> dumpJournal j

-- Generate all possible single-threaded executions from the concurrent history.
interleavings :: History -> Forest (Command, Response)
interleavings (History [])  = []
interleavings (History ops) =
  [ Node (cmd, resp) (interleavings (History ops'))
  | (tid, cmd)   <- takeInvocations ops
  , (resp, ops') <- findResponse tid
                      (filter1 (not . matchInvocation tid) ops)
  ]
  where
    takeInvocations :: [Operation] -> [(Pid, Command)]
    takeInvocations []                         = []
    takeInvocations ((Invoke pid cmd)   : ops) = (pid, cmd) : takeInvocations ops
    takeInvocations ((Ok    _pid _resp) : _)   = []

    findResponse :: Pid -> [Operation] -> [(Response, [Operation])]
    findResponse _pid []                                   = []
    findResponse  pid ((Ok pid' resp) : ops) | pid == pid' = [(resp, ops)]
    findResponse  pid (op             : ops)               =
      [ (resp, op : ops') | (resp, ops') <- findResponse pid ops ]

    matchInvocation :: Pid -> Operation -> Bool
    matchInvocation pid (Invoke pid' _cmd) = pid == pid'
    matchInvocation _   _                  = False

    filter1 :: (a -> Bool) -> [a] -> [a]
    filter1 _ []                   = []
    filter1 p (x : xs) | p x       = x : filter1 p xs
                       | otherwise = xs


-- If any one of the single-threaded executions respects the state machine
-- model, then the concurrent execution is correct.
linearisable :: Forest (Command, Response) -> Bool
linearisable = any' (go initModel)
  where
    go :: Model -> Tree (Command, Response) -> Bool
    go model (Node (cmd, resp) ts) =
      -- any because we want at least one to succeed here, `any'` would be wrong
      any (\(model', resp') -> resp == resp' && any' (go model') ts)
          (step' cmd model)

    any' :: (a -> Bool) -> [a] -> Bool
    any' _p [] = True
    any'  p xs = any p xs

prop_concurrent :: Property
prop_concurrent = mapSize (min 20) $ noShrinking $
  forAllConcProgram $ \(ConcProgram cmdss) -> monadicIO $ do
    monitor (classifyCommandsLength (concat cmdss))
    -- Rerun a couple of times, to avoid being lucky with the interleavings.
    monitor (tabulate "Commands" (map constructorString (concat cmdss)))
    replicateM_ 10 $ do
      (fp, jour) <- run initJournal
      queue <- run newTQueueIO
      run (mapM_ (mapConcurrently (concExec queue jour)) cmdss)
      hist <- History <$> run (atomically (flushTQueue queue))
      run (removeFile fp)
      assertWithFail (linearisable (interleavings hist)) (prettyHistory hist)
      written <- run (metricsBytesWritten jour)
      monitor (classifyBytesWritten written)

------------------------------------------------------------------------

data PidStatus
  = DoingNothing
  | MadeRequest Command
  | CommittedRequest Command Response -- we don't support :INFO, or failures so
                                      -- we will always have response

-- `selectOne` will pick one element from the list, and return all other
-- elements (in no specific order)
selectOne :: [a] -> Gen (a,[a])
selectOne = elements . gens []
  where
    gens :: [a] -> [a] -> [(a,[a])]
    gens before [] = []
    gens before (a:rest) = (a,before ++ rest) : gens (a:before) rest

-- generate a `History` and the linearised history `[Command]`
genHistory :: [Pid] -> Model -> Gen (History, [Command])
genHistory pids initModel = sized $ \s ->
  go (History []) [] initModel s $ zip pids (repeat DoingNothing)
  where
    consH x (History xs) = History (x:xs)
    go :: History -> [Command] -> Model -> Int -> [(Pid, PidStatus)]
       -> Gen (History, [Command])
    go (History conc) linear model size [] = pure (History $ reverse conc, reverse linear)
    go conc linear model size pids = do
      ((pid, state), pids') <- selectOne pids
      case state of
        DoingNothing
          | size <= 0 -> -- we are done generating commands
            go conc linear model size pids'
          | otherwise -> do -- generate command
            cmd <- genCommand
            go (consH (Invoke pid cmd) conc) linear model (pred size)
              ((pid, MadeRequest cmd):pids')
        MadeRequest cmd -> do -- request succeed, and response succeed
          let (model', resp) = step cmd model
          go conc (cmd:linear) model' size ((pid, CommittedRequest cmd resp):pids')
        CommittedRequest cmd resp -> do -- request returns
          go (consH (Ok pid resp) conc) linear model size ((pid, DoingNothing):pids')

prop_lineariseIsOkay :: Property
prop_lineariseIsOkay = mapSize (min 20) $
  -- ^ Above 20 it starts to take too much time to check.
  forAll (genHistory pids initModel) $ \(history, _) ->
    linearisable (interleavings history)
  where
    pids = map Pid [0..5]
