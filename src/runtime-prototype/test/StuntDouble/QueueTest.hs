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
  | Move [Int]
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
step (Move xs) m
  | length xs + length (mQueue m) <= mCapacity m = (m { mQueue = mQueue m ++ xs}, Bool True)
  | otherwise = (m, Bool False)

exec :: Command -> Queue Int -> IO Response
exec Size        q = Int   <$> size q
exec Clear       q = Unit  <$> clear q
exec IsEmpty     q = Bool  <$> isEmpty q
exec (Enqueue x) q = Bool  <$> enqueue x q
exec Dequeue     q = Maybe <$> dequeue q
exec (Move xs)   q = do
  q' <- newQueue (length xs)
  enqueues xs q
  Bool <$> move q q'

genCommand :: Int -> Int -> Gen Command
genCommand cap sz = frequency
  [ (3, pure Size)
  , (0, pure Clear) -- NOTE: If this happens too often, it causing enqueue to
                    -- rarely write to a full queue.
  , (2, pure IsEmpty)
  , (5, Enqueue <$> arbitrary)
  , (2, pure Dequeue)
  , (1, genMove)
  ]
  where
    genMove = do
      -- `cap - sz` will fit, when `n` is negative will also fit, and when `n`
      -- is positive won't fit.
      let is = [ i | i <- [0..cap], cap - sz - i >= 0 ]
      -- We append `[0]` because if `is` is empty elements fails otherwise.
      n <- elements (is ++ map negate is ++ [0])
      Move <$> vectorOf (cap - sz + n) arbitrary

genCommands :: Int -> Int -> Gen [Command]
genCommands cap sz = go sz 20 -- sized (go sz)
  where
    go _sz 0 = return []
    go sz  n = do
      cmd <- genCommand cap sz
      let sz' = case cmd of
                  Size       -> sz
                  Clear      -> 0
                  IsEmpty    -> sz
                  Enqueue {} -> sz + 1
                  Dequeue    -> sz - 1
                  Move xs    -> sz + length xs
      cmds <- go sz' (n - 1)
      return (cmd : cmds)

newtype Capacity = Capacity Int
  deriving Show

instance Arbitrary Capacity where
  arbitrary = Capacity <$> choose (0, 5)

prop_queue :: Capacity -> Property
prop_queue (Capacity cap) =
  forAllShrink (genCommands cap 0) (shrinkList (const [])) $ \cmds -> monadicIO $ do
    let m = newModel cap
    q <- run (newQueue cap)
    monitor (tabulate "Commands" (map prettyCommand cmds))
    (result, hist) <- go cmds m q []
    return result
    mapM_ (monitor . classify') (zip cmds hist)
    where
      go []          _m _q hist = return (True, reverse hist)
      go (cmd : cmds) m  q hist = do
        let (m', resp) = step cmd m
        resp' <- run (exec cmd q)
        unless (resp == resp') $
          monitor (counterexample (show resp ++ " /= " ++ show resp'))
        go cmds m' q (resp : hist)

      classify' :: (Command, Response) -> Property -> Property
      classify' (Enqueue {}, Bool b) = classify b "enqueue successful"
      classify' (Move {},    Bool b) = classify b "move successful"
      classify' (_, _)               = id
