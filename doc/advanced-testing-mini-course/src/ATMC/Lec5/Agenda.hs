{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module ATMC.Lec5.Agenda where

import Data.Heap (Entry(Entry), Heap)
import qualified Data.Heap as Heap
import Data.List (foldl')

import ATMC.Lec5.Time

------------------------------------------------------------------------

newtype Agenda e = Agenda (Heap (Entry Time e))
  deriving newtype (Semigroup, Monoid)
  deriving stock Show

emptyAgenda :: Agenda e
emptyAgenda = Agenda Heap.empty

union :: Agenda e -> Agenda e -> Agenda e
union (Agenda h) (Agenda h') = Agenda (h `Heap.union` h')

fromList :: [(Time, e)] -> Agenda e
fromList = Agenda . Heap.fromList . map (uncurry Entry)

pop :: Agenda e -> Maybe ((Time, e), Agenda e)
pop (Agenda h) = case Heap.uncons h of
  Nothing              -> Nothing
  Just (Entry t e, h') -> Just ((t, e), Agenda h')

push :: (Time, e) -> Agenda e -> Agenda e
push (t, e) (Agenda h) = Agenda (Heap.insert (Entry t e) h)

pushList :: [(Time, e)] -> Agenda e -> Agenda e
pushList tes (Agenda h) =
  Agenda (foldl' (\ih (t, e) -> Heap.insert (Entry t e) ih) h tes)
