{-# LANGUAGE OverloadedStrings #-}

module JournalTest where

import Control.Monad (unless)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Test.Tasty.HUnit (Assertion, assertBool)
import Data.Vector (Vector)
import qualified Data.Vector as Vector
import System.Directory (removePathForcibly)
import System.Timeout (timeout)
import Test.QuickCheck
import Test.QuickCheck.Instances.ByteString ()
import Test.QuickCheck.Monadic

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

valid :: Command -> Bool
valid ReadJournal = True
valid (AppendBS bs) = not $ BS.null bs

prettyCommand :: Command -> String
prettyCommand AppendBS    {} = "AppendBS"
prettyCommand ReadJournal {} = "ReadJournal"

shrinkCommand :: Command -> [Command]
shrinkCommand ReadJournal = []
shrinkCommand (AppendBS bs) = filter valid
  [ AppendBS bs' | bs' <- shrink bs]

data Response
  = Unit ()
  | ByteString ByteString
  deriving (Eq, Show)

type Model = FakeJournal

step :: Command -> Model -> (Model, Response)
step (AppendBS bs) m = Unit <$> appendBSFake bs m
step ReadJournal   m = ByteString <$> readJournalFake m

exec :: Command -> (Journal, JournalConsumer) -> IO Response
exec (AppendBS bs) (j, _jc) = Unit <$> appendBS j bs
exec ReadJournal   (_j, jc) = ByteString <$> readJournal jc

genCommand :: Model -> Gen Command
genCommand m
  -- If there's nothing new to read, then don't generate reads (because they are
  -- blocking).
  | Vector.length (fjJournal m) == fjIndex m = genAppendBS
  | otherwise = frequency
    [ (1, genAppendBS)
    , (1, pure ReadJournal)
    ]
  where
    genAppendBS :: Gen Command
    genAppendBS =
      AppendBS <$> arbitrary
        -- NOTE: We can only append non-empty bytestrings, that's a
        -- pre-condition.
        `suchThat` (\bs -> not (BS.null bs))

genCommands :: Model -> Gen [Command]
genCommands m = sized go
  where
    go 0 = return []
    go n = do
      cmd <- genCommand m
      cmds <- go (n - 1)
      return (cmd : cmds)

prop_genCommandsValid :: Property
prop_genCommandsValid = forAllShrink (genCommands m) (const []) $ all valid
  where m = startJournalFake

prop_genCommandsShrinkValid :: Property
prop_genCommandsShrinkValid = forAllShrink (genCommands m) (const []) $ \ cmds ->
  all valid cmds ==> all (all valid . shrinkCommand) cmds
  where m = startJournalFake

prop_journal :: Property
prop_journal =
  let m = startJournalFake in
  forAllShrink (genCommands m) (shrinkList shrinkCommand) $ \cmds ->
   collect ("rotations: " <> show (nrRotations cmds defaultOptions)) $ monadicIO $ do
    run (putStrLn ("Generated commands: " ++ show cmds))
    run (removePathForcibly "/tmp/journal-test")
    jjc <- run (startJournal "/tmp/journal-test" defaultOptions)
    monitor (tabulate "Commands" (map prettyCommand cmds))
    (result, hist) <- go cmds m jjc []
    return result
    mapM_ (monitor . classify') (zip cmds hist)
    where
      go []          _m _jjc hist = return (True, reverse hist)
      go (cmd : cmds) m  jjc hist = do
        let (m', resp) = step cmd m
        resp' <- run (exec cmd jjc)
        assertWithFail (resp == resp') $
          show resp ++ " /= " ++ show resp'
        go cmds m' jjc (resp : hist)

      nrRotations xs (Options maxB)= sum [ howMuchAppend c | c <- xs] `div` maxB
        where
          howMuchAppend ReadJournal = 0
          howMuchAppend (AppendBS bs) = BS.length bs

      classify' :: (Command, Response) -> Property -> Property
      classify' (_, _) = id
      -- classify' (Enqueue {},     Bool b) = classify b "enqueue successful"
      -- classify' (EnqueueList {}, Bool b) = classify b "enqueueList successful"
      -- classify' (Move {},        Bool b) = classify b "move successful"

      assertWithFail :: Monad m => Bool -> String -> PropertyM m ()
      assertWithFail condition msg = do
        unless condition $
          monitor (counterexample ("Failed: " ++ msg))
        assert condition

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
