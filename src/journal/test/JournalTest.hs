module JournalTest where

import Control.Monad (unless)
import Data.ByteString (ByteString)
import Test.QuickCheck
import Test.QuickCheck.Monadic

import Journal

------------------------------------------------------------------------

newtype FakeJournal = FakeJournal [ByteString]

newtype FakeIndex = FakeIndex Int

newFakeJournal :: FakeJournal
newFakeJournal = FakeJournal []

journalFake :: ByteString -> FakeJournal -> FakeJournal
journalFake = undefined

journalManyFake :: [ByteString] -> FakeJournal -> FakeJournal
journalManyFake = undefined

truncateFake :: FakeJournal -> FakeJournal
truncateFake = undefined

replayFake :: FakeIndex -> FakeJournal -> (ByteString -> a) -> [a]
replayFake = undefined

------------------------------------------------------------------------

data Command
  = JournalCmd ByteString
  | JournalManyCmd [ByteString]
  | TruncateCmd -- XXX: Index
  | ReplayCmd -- XXX: Index
  deriving Show

prettyCommand :: Command -> String
prettyCommand = undefined

data Response = R
  deriving (Eq, Show)

type Model = FakeJournal

step :: Command -> Model -> (Model, Response)
step = undefined

exec :: Command -> Journal -> IO Response
exec = undefined

genCommand :: Model -> Gen Command
genCommand = undefined

genCommands :: Model -> Gen [Command]
genCommands = undefined

prop_journal :: Property
prop_journal =
  forAllShrink (genCommands newFakeJournal) (shrinkList (const [])) $ \cmds -> monadicIO $ do
    let m = newFakeJournal
    (j, _jc) <- run (startJournal "/tmp/journal-test" defaultOptions)
    monitor (tabulate "Commands" (map prettyCommand cmds))
    (result, hist) <- go cmds m j []
    return result
    mapM_ (monitor . classify') (zip cmds hist)
    where
      go []          _m _j hist = return (True, reverse hist)
      go (cmd : cmds) m  j hist = do
        let (m', resp) = step cmd m
        resp' <- run (exec cmd j)
        unless (resp == resp') $
          monitor (counterexample (show resp ++ " /= " ++ show resp'))
        go cmds m' j (resp : hist)

      classify' :: (Command, Response) -> Property -> Property
      classify' = undefined
      -- classify' (Enqueue {},     Bool b) = classify b "enqueue successful"
      -- classify' (EnqueueList {}, Bool b) = classify b "enqueueList successful"
      -- classify' (Move {},        Bool b) = classify b "move successful"
      -- classify' (_, _)                   = id
