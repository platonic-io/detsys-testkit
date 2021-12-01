module JournalTest where

import Control.Monad (unless)
import Data.ByteString (ByteString)
import Test.QuickCheck
import Test.QuickCheck.Monadic

import Journal

------------------------------------------------------------------------

-- scenarios:
--  1. start new journal
--   1a. Use `mmapFilePtr :: FilePath -> Mode -> Maybe (Int64, Int) -> IO (Ptr a, Int, Int, Int)`
--       to memory map the active journal file to a pointer.
--  2. journal a bunch of entries
--    2a. The application listen on socket and calls `recvBuf :: Socket -> Ptr Word8 -> Int -> IO Int`;
--    2b. First it recevies a header into some buffer (the pointer) telling us
--        how long the message is;
--    2c. Next we call recvBuf again this time using the memory mapped pointer
--        and length we got from the header, to read the message straigt into
--        the journal.
--    2d. Finally write the header to the journal.

--  3. take a snapshot of application state at some position of the journal
--  4. truncate journal until above position
--  5. journal some more entries
--  6. crash/restart happens
--  7. restore journal (fix inconsistencies and restore journal datatype from filesystem)
--  8. load snapshot and position of snapshot
--  9. replay from position to restore application state
-- 10. journaling can resume at this point (we can't journal while replaying)

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

{-
rop_journal :: Property
rop_journal =
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

-}
