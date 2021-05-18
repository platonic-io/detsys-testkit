module StuntDouble.Datatype where

import Data.Time
import Data.Heap (Heap)
import qualified Data.Heap as Heap
import Data.Map (Map)
import Data.Text

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
  deriving (Eq, Ord, Show)

------------------------------------------------------------------------

plus :: Datatype -> Datatype -> Datatype
plus (Integer i) (Integer j) = Integer (i + j)

pop :: Datatype -> Datatype
pop (Heap h) = case Heap.uncons h of
  Nothing      -> None
  Just (x, h') -> Some (Pair (Heap.payload x) (Heap h'))

push :: Datatype -> Datatype -> Datatype -> Datatype
push p x (Heap h) = Heap (Heap.insert (Heap.Entry p x) h)
