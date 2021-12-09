{-# LANGUAGE OverloadedStrings #-}

module JournalTest where

import Control.Exception (SomeException, throwIO, catch)
import Control.Monad (unless)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Monoid (Sum(Sum))
import Data.Vector (Vector)
import qualified Data.Vector as Vector
import System.Directory (removePathForcibly)
import System.Timeout (timeout)
import Test.QuickCheck
import Test.QuickCheck.Instances.ByteString ()
import Test.QuickCheck.Monadic
import Test.Tasty.HUnit (Assertion, assertBool)

import Journal
import Journal.Internal

------------------------------------------------------------------------

data FakeJournal = FakeJournal
  { fjJournal :: Vector ByteString
  , fjIndex   :: Int
  }
  deriving Show

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

prettyCommand :: Command -> String
prettyCommand AppendBS    {} = "AppendBS"
prettyCommand ReadJournal {} = "ReadJournal"

data Response
  = Unit ()
  | ByteString ByteString
  deriving (Eq, Show)

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
      , (1, return (BS.replicate (oMaxByteSize testOptions - hEADER_SIZE) (fromIntegral i)))
      , (1, return (BS.replicate (oMaxByteSize testOptions - hEADER_SIZE - 1)
                                 (fromIntegral i)))
      ]

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
    monitor (tabulate "Commands" (map prettyCommand cmds))
    (result, hist) <- go cmds m jjc []
    run (uncurry stopJournal jjc)
    return result
    monitorStats (stats (zip cmds hist))
    where
      go []          _m _jjc hist = return (True, reverse hist)
      go (cmd : cmds) m  jjc hist = do
        let (m', resp) = step cmd m
        resp' <- run (exec cmd jjc)
        assertWithFail (resp == resp') $
          show resp ++ " /= " ++ show resp'
        go cmds m' jjc (resp : hist)

      assertWithFail :: Monad m => Bool -> String -> PropertyM m ()
      assertWithFail condition msg = do
        unless condition $
          monitor (counterexample ("Failed: " ++ msg))
        assert condition

data Stats = Stats
  { sBytesWritten :: Int
  , sRotations    :: Int
  }
  deriving Show

stats :: [(Command, Response)] -> Stats
stats hist = Stats
  { sBytesWritten = totalAppended
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
      putStrLn (show m)
      putStrLn ""
      putStrLn ("    == " ++ show cmd ++ " ==> " ++ show resp)
      putStrLn ""
      if null cmds
      then putStrLn (show m')
      else return ()
      resp' <- exec cmd jjc `catch` handleException (fst jjc) hist
      if resp == resp'
      then go m' jjc cmds ((cmd, resp) : hist)
      else do
        putStrLn ""
        putStrLn ("Failed: " ++ show resp ++ " /= " ++ show resp')
        putStrLn ""
        putStrLn "Journal dump:"
        dumpJournal (fst jjc)
        print (stats (reverse hist))
        return False

    handleException :: Journal -> [(Command, Response)] -> SomeException -> IO a
    handleException jour hist ex = do
      putStrLn "Journal dump:"
      dumpJournal jour
      print (stats (reverse hist))
      error ("Failed, threw exception: " ++ show ex)

------------------------------------------------------------------------

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
unit_bug2 = assertProgram "rotation without padding"
  [ AppendBS "IIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIII" -- 122 + 6 = 128 bytes
  , AppendBS "YYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYY" -- 121 + 6 = 127 bytes
  , ReadJournal
  ]

unit_bug3 :: Assertion
unit_bug3 = assertProgram "segfault"
  [ AppendBS "PPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPP" -- 121 + 6 = 127 bytes
  , AppendBS "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA" -- 122 + 6 = 128 bytes
  , AppendBS "DDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDD" -- 122 + 6 = 128 bytes
  , AppendBS "EEEEEE"
  , ReadJournal
  , ReadJournal
  ]

------------------------------------------------------------------------

assertProgram :: String -> [Command] -> Assertion
assertProgram msg cmds = do
  b <- runCommands cmds
  assertBool msg b
