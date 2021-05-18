module StuntDouble.Datatype where

import Data.Heap (Heap)
import Data.Map (Map)
import Data.Text

------------------------------------------------------------------------

data Datatype
  = Unit ()
  | Integer Integer
  | Double Double
  | Bool Bool
  | Text Text
  | Enum [Text]
  | Pair Datatype Datatype
  | Inl Datatype
  | Inr Datatype
  -- | Timestamp UTCTime
  | Map (Map Datatype Datatype)
  | List [Datatype]
  | Heap (Heap Datatype)
  deriving Show

------------------------------------------------------------------------

plus :: Datatype -> Datatype -> Datatype
plus (Integer i) (Integer j) = Integer (i + j)
