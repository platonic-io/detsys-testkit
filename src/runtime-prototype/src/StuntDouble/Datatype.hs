module StuntDouble.Datatype where

import Data.Time
import Data.Heap (Heap)
import qualified Data.Heap as Heap
import Data.Map (Map)
import Data.Text (Text)

------------------------------------------------------------------------

data Datatype
  = Unit ()
  | Integer Integer
  | Double Double
  | Bool Bool
  | Text Text
  | Enum Text
  | Pair Datatype Datatype
  | Inl Datatype
  | Inr Datatype
  | Timestamp UTCTime
  | None
  | Some Datatype
  | Map (Map Datatype Datatype)
  | List [Datatype]
  | Heap (Heap (Heap.Entry Datatype Datatype))
  deriving (Eq, Ord, Show, Read)

------------------------------------------------------------------------

plus :: Datatype -> Datatype -> Datatype
plus (Integer i) (Integer j) = Integer (i + j)

emptyHeap :: Datatype
emptyHeap = Heap Heap.empty

heapFromList :: [(Datatype, Datatype)] -> Datatype
heapFromList = Heap . Heap.fromList . map (\(p, x) -> Heap.Entry p x)

pop :: Datatype -> Datatype
pop (Heap h) = case Heap.uncons h of
  Nothing      -> None
  Just (x, h') -> Some (Pair (Heap.payload x) (Heap h'))

push :: Datatype -> Datatype -> Datatype -> Datatype
push p x (Heap h) = Heap (Heap.insert (Heap.Entry p x) h)

epoch :: Datatype
epoch = Timestamp (UTCTime (toEnum 0) 0)

------------------------------------------------------------------------

{-
class Convertible a where
  convertFrom :: a -> Datatype
  convertTo   :: Datatype -> a

instance Convertible Integer where
  convertFrom = Integer
  convertTo (Integer i) = i

instance Convertible Text where
  convertFrom = Text
  convertTo (Text t) = t

instance Convertible UTCTime where
  convertFrom = Timestamp
  convertTo (Timestamp t) = t

instance (Convertible a, Convertible b) => Convertible (a, b) where
  convertFrom (x, y)     = Pair (convertFrom x) (convertFrom y)
  convertTo   (Pair x y) = (convertTo x, convertTo y)

instance Convertible a => Convertible (Maybe a) where
  convertFrom Nothing = None
  convertFrom (Just x) = Some (convertFrom x)

  convertTo None = Nothing
  convertTo (Some x) = Just (convertTo x)

instance Convertible Datatype where
  convertFrom = id
  convertTo = id

(^.) :: Convertible a => Text -> (Datatype -> Datatype) -> Free ActorF a
k ^. a = do
  s <- getState
  return (convertTo (a (getField k s)))

(.=) :: Convertible a => Text -> a -> Free ActorF ()
k .= v = do
  s <- getState
  putState (setField k (convertFrom v) s)

(%=) :: (Datatype -> Datatype) -> Text -> Free ActorF ()
f %= k = do
  s <- getState
  putState (modifyField k f s)

op2 :: (Convertible a, Convertible b)
    => (Datatype -> Datatype -> Datatype -> Datatype) -> a -> b -> (Datatype -> Datatype)
op2 f x y = f (convertFrom x) (convertFrom y)

get :: Convertible a => Text -> Free ActorF a
get k = do
  s <- getState
  return (convertTo (getField k s))

genArrivalTime :: UTCTime -> Integer -> Free ActorF UTCTime
genArrivalTime = undefined
-}
