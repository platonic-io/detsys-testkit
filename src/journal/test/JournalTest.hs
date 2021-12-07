{-# LANGUAGE OverloadedStrings #-}

module JournalTest where

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
precondition m (AppendBS bs) = not (BS.null bs)

step :: Command -> Model -> (Model, Response)
step (AppendBS bs) m = Unit <$> appendBSFake bs m
step ReadJournal   m = ByteString <$> readJournalFake m

exec :: Command -> (Journal, JournalConsumer) -> IO Response
exec (AppendBS bs) (j, _jc) = Unit <$> appendBS j bs
exec ReadJournal   (_j, jc) = ByteString <$> readJournal jc

genCommand :: Gen Command
genCommand = frequency
  [ (1, AppendBS <$> arbitrary)
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
shrinkCommand (AppendBS bs) = [ AppendBS bs' | bs' <- shrink bs, not (BS.null bs') ]

shrinkCommands :: Model -> [Command] -> [[Command]]
shrinkCommands m = filter (validProgram m) . shrinkList shrinkCommand

validProgram :: Model -> [Command] -> Bool
validProgram = go True
  where
    go False _m _cmds       = False
    go valid _m []          = valid
    go valid m (cmd : cmds) = go (precondition m cmd) (fst (step cmd m)) cmds

prop_journal :: Property
prop_journal =
  let
    m    = startJournalFake
    opts = defaultOptions
  in
  forAllShrink (genCommands m) (shrinkCommands m) $ \cmds -> monadicIO $ do
    -- run (putStrLn ("Generated commands: " ++ show cmds))
    run (removePathForcibly "/tmp/journal-test")
    jjc <- run (startJournal "/tmp/journal-test" opts)
    monitor (tabulate "Commands" (map prettyCommand cmds))
    (result, hist) <- go cmds m jjc []
    return result
    monitor (stats opts (zip cmds hist))
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

stats :: Options -> [(Command, Response)] -> Property -> Property
stats opts hist =
  collect ("Rotations: " <> show (totalAppended `div` oMaxByteSize opts))
  where
    Sum totalAppended =
      foldMap (\(cmd, _resp) ->
                 case cmd of
                   AppendBS bs -> Sum (BS.length bs)
                   _otherwise  -> mempty) hist

runCmds :: [Command] -> IO Bool
runCmds cmds = do
  let m = startJournalFake
  removePathForcibly "/tmp/journal-test"
  jjc <- startJournal "/tmp/journal-test" defaultOptions
  putStrLn ""
  go m jjc cmds
  where
    go m jjc [] = return True
    go m jjc (cmd : cmds) = do
      let (m', resp) = step cmd m
      putStrLn (show m)
      putStrLn ""
      putStrLn ("    == " ++ show cmd ++ " ==> " ++ show resp)
      putStrLn ""
      if null cmds
      then putStrLn (show m')
      else return ()
      resp' <- exec cmd jjc
      if resp == resp'
      then go m' jjc cmds
      else do
        putStrLn ""
        putStrLn ("Failed: " ++ show resp ++ " /= " ++ show resp')
        putStrLn ""
        putStrLn "Journal dump:"
        dumpJournal (fst jjc)
        return False

------------------------------------------------------------------------

assertBoolM :: String -> IO Bool -> Assertion
assertBoolM msg io = do
  b <- io
  assertBool msg b
