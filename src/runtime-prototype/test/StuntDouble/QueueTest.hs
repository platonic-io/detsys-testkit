module StuntDouble.QueueTest where

import Control.Monad
import Test.QuickCheck
import Test.QuickCheck.Monadic

import StuntDouble.Queue

------------------------------------------------------------------------

data Model = Model
  { mCapacity :: Int
  , mQueue    :: [Int]
  }
  deriving Show

newModel :: Int -> Model
newModel cap = Model cap []

data Command
  = Size
  | Clear
  | IsEmpty
  | Enqueue Int
  | Dequeue
  | Move Model
  deriving Show

prettyCommand :: Command -> String
prettyCommand Size       = "Size"
prettyCommand Clear      = "Clear"
prettyCommand IsEmpty    = "IsEmpty"
prettyCommand Enqueue {} = "Enqueue"
prettyCommand Dequeue    = "Dequeue"
prettyCommand Move {}    = "Move"

data Response
  = Int Int
  | Unit ()
  | Maybe (Maybe Int)
  | Bool Bool
  deriving (Show, Eq)

step :: Command -> Model -> (Model, Response)
step Size        m = (m, Int (length (mQueue m)))
step Clear       m = (m { mQueue = [] }, Unit ())
step IsEmpty     m = (m, Bool (null (mQueue m)))
step (Enqueue x) m
  | length (mQueue m) >= mCapacity m = (m, Bool False)
  | otherwise                        = (m { mQueue = mQueue m ++ [x]}, Bool True)
step Dequeue     m = case mQueue m of
  []       -> (m, Maybe Nothing)
  (x : xs) -> (m { mQueue = xs }, Maybe (Just x))
step (Move m') m = undefined

exec :: Command -> Queue Int -> IO Response
exec Size        q = Int   <$> size q
exec Clear       q = Unit  <$> clear q
exec IsEmpty     q = Bool  <$> isEmpty q
exec (Enqueue x) q = Bool  <$> enqueue x q
exec Dequeue     q = Maybe <$> dequeue q

genCommand :: Gen Command
genCommand = frequency
  [ (3, pure Size)
  , (1, pure Clear)
  , (2, pure IsEmpty)
  , (3, Enqueue <$> arbitrary)
  , (2, pure Dequeue)
  -- XXX: Move, probably need to pass in cap and sz in order to generate valid
  -- models to move...
  ]

genCommands :: Int -> Int -> Gen [Command]
genCommands cap sz = go sz (5 * cap)
  where
    go _sz 0 = return []
    go sz  n = do
      cmd <- genCommand
      let sz' = case cmd of
                  Clear      -> 0
                  IsEmpty    -> sz
                  Enqueue {} -> sz + 1
                  Dequeue    -> sz - 1
                  Move m'    -> sz + length (mQueue m')
      cmds <- go sz' (n - 1)
      return (cmd : cmds)

prop_queue :: Positive (Small Int) -> Property
prop_queue (Positive (Small cap)) =
  forAllShrink (genCommands cap 0) (shrinkList (const [])) $ \cmds -> monadicIO $ do
    let m = newModel cap
    q <- run (newQueue cap)
    monitor (tabulate "Commands" (map prettyCommand cmds))
    go cmds m q
    where
      go []          _m _q = return True
      go (cmd : cmds) m  q = do
        let (m', resp) = step cmd m
        resp' <- run (exec cmd q)
        unless (resp == resp') $
          monitor (counterexample (show resp ++ " /= " ++ show resp'))
        go cmds m' q
